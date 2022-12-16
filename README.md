## About

I created this repo by stripping out (most of) the application specific logic of
my first IHP & Elm (with elm-ui) app [xcuseme](https://github.com/unterkoefler/xcuseme). There's
probably a good amount of xcuseme stuff left (prs welcome!), but this should be a decent starting place. You'll get:
- Users and auth (elm-ui login and signup forms)
- An about page
- A navbar
- A widget system as described by [this excellent blog series](https://www.haskellpreneur.com/articles/ihp-with-elm/1-get-elm-working-with-ihp/) (thank u!!)

## Get started

1. Clone this repo
2. Reset git history:
    ```
    rm -Rf .git
    git init
    git add .
    git commit -m "initial commit"
    ```
3. Run `./start`
4. Start working through TODOs


To generate new elm types:

```
nix-shell --run "yarn run gen-types"
```
