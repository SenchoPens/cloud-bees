# cloud-bees
Tutorial (in Russian) on Cloud Haskell using the code from this repository: https://habrahabr.ru/company/goto/blog/351496/

## Install & Build
```shell
git clone https://github.com/SenchoPens/cloud-bees.git
cd cloud-bees
stack setup  # Stack will install GHC
stack build  # Compile
```

## Run
Execute the following line in one terminal:
```shell
stack exec cloud-bees-exe 9000 9001 2>/dev/null
```
And this line in another terminal:
```shell
stack exec cloud-bees-exe 9001 9000 2>/dev/null
```
After this, you will have 2 terminals running a REPL. It supports 2 commands: `Add (x, y)` to add a flower and `Show` to list known flowers. 
