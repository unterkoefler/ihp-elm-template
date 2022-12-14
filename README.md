1. Clone this repo
2. Reset git history:
    ```
    rm -Rf .git
    git init
    git add .
    git commit -m "initial commit"
    ```
3. Run `./start`


To generate new elm types:

```
nix-shell --run "yarn run gen-types"
```
