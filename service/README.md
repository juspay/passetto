# Passetto-service

Encryption server.

## Build

This project requires [`libsodium`](https://libsodium.gitbook.io/doc/installation) of version `1.0.18` to be installed.

Then you can build

* with stack: `stack build`.

## Setup

### Initialising keystorage

To generate keys, run

```sh
stack exec passetto-init -- <password> <number of keys>
```

or

```sh
stack exec passetto-init-interactive -- addkeys <number of keys>
```

The `-interactive` version allows the user to enter the password interactively from the keyboard.

If the storage isn't yet initialized, it becomes initialized with the password supplied (becoming storage current password) and the set of fresh `<number of keys>`.

If the storage is already initialized, the password supplied is checked against storage current password and if ok, the set of fresh `<number of keys>` is generated and added to the storage.

Keys are stored in [acid-state](https://hackage.haskell.org/package/acid-state) database which is kept in `state` folder of the working directory.

### Specifying TLS certificate

Running the server properly requires a valid TLS certificate, make sure to prepare one.

## Running server

Encryption server can be run as follows:

```sh
stack exec passetto-server
```

Parameters are accepted via environmental variables:

* `MASTER_PASSWORD` - password for decrypting master key.
* `PORT` - port to serve at. Default is 8012.
* `USE_TLS` flag - accept HTTPS connections and only them.
* `TLS_KEY` and `TLS_CERT` - path to TLS key and certificate (viable when `USE_TLS` is set). By default, self-signed certificate from `./cert` folder is used.

### Usage example

Examples provided using [httpie](https://httpie.org/docs) utility.

Further we assume the app running in testing enviroment at 8012 port.

#### Checking service status:

```sh
> http :8012/status
HTTP/1.1 200 OK
{
    "keys_number": 5
}
```

#### Encrypting a value

```sh
> http :8012/encrypt value='My text'
HTTP/1.1 200 OK
{
    "value": "0.1.0|3|G8iq1rbeyA+IvM9EIe7beYEi33JEwSm+q+d+Iww8Xk8pqdbGM+4WvuGu4tKk5NCazGu3WovX1hs="
}
```

#### Decrypting a value

```sh
> echo '{ "value": "..." }' | http :8012/decrypt
HTTP/1.1 200 OK
{
    "value": "My text"
}
```

#### Encrypting an object

```sh
> echo '{ "value": { "name": "User", "phone": 123 } }' \
  | http :8012/encrypt/obj
HTTP/1.1 200 OK
{
    "value": {
        "name": "0.1.0|0|7cGcOgDAJETHmvjXEvponxger2L+QZOPRcXfToomqxZ9myXRpMU1VWUnwVNCzahV+0EH4WU=",
        "phone": "0.1.0|1|pQyOJEiXPOH69UfD4QBvfWPx50z1COiQ0Djnw48yM24sYr3sP3lQFRTtxB9GYT+vEBHiLBMZ"
    }
}
```

#### Decrypting an object

```sh
> echo '{ "value": { "name": "...", "phone": "..." } }' \
  | http :8012/decrypt/obj
HTTP/1.1 200 OK
{
    "value": {
        "name": "User",
        "phone": "123"
    }
}
```

## Implementation considerations

This project is mostly a port of [another encryption service](https://github.com/project-sunbird/enc-service/).

However, that service contains some security flaws as described in [Passetto cryptography analysis by Serokell](https://www.notion.so/Passetto-cryptography-076da8b7c05f4d519d86b434d7a5efa0), which we address here via using high-level crypto library [Sodium](https://libsodium.org):

1. Replaced the custom symmetric encryption code with the use of the [`crypto_secretbox`](https://nacl.cr.yp.to/secretbox.html) primitive, that provides authenticated secret-key encryption.
The underlying low-level primitives are the [Salsa20 stream cypher](https://en.wikipedia.org/wiki/Salsa20) and [Poly1305 MAC](https://en.wikipedia.org/wiki/Poly1305) (see [“Cryptography in NaCl”](https://nacl.cr.yp.to/valid.html) for details).

2. Derived the symmetric encryption key from password using the [Argon2id](https://en.wikipedia.org/wiki/Argon2) algorithm.
It is mostly what [`crypto_pwhash`](https://doc.libsodium.org/password_hashing/default_phf#key-derivation) [key derivation function](https://en.wikipedia.org/wiki/Key_derivation_function) does.
It is implemented in Sodium, though lacks Haskell bindings yet.

3. Replaced the custom code that uses RSA with the [`crypto_box`](https://nacl.cr.yp.to/box.html) high-level primitive, which provides authenticated public-key encryption.
The underlying low-level primitives are [Curve25519](https://en.wikipedia.org/wiki/Curve25519), which is used in [elliptic-curve Diffie–Helman](https://en.wikipedia.org/wiki/Elliptic-curve_Diffie%E2%80%93Hellman) to establish a shared symmetric session key, and the message is then encrypted and authenticated as above with Salsa20 and Poly1305 (see [“Cryptography in NaCl”](https://nacl.cr.yp.to/valid.html) for details).
