# Passetto-client

To work with passetto server from Haskell use [`Passetto.Client`](./src/Passetto/Client.hs) module.

Example of client call:

```haskell
λ> withPassettoCtx (mkDefPassettoContext "localhost" 8012) $ cliEncrypt @(Encrypted Int) 123
Right (Encrypted {unEncrypted = "0.1.0|3|UR4CY0+0v/8aYXbAGRtyumpkC/arM9+VAeOa+jYzjhfU3w37VjXc6C/D1iC9UC/51lM95LuQ"})
```
