let Package = (../types.dhall).Package
in
[
  Package :: {
    pname = "hello",
    version = "0.1",
    files = ["tests/hello.el", "tests/hello-util.el"],
    dependencies = ["dash"],
    mainFile = Some "tests/hello.el",
    recipe = ''
    (hello :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/hello.el" "tests/hello-util.el"))
    ''
  },
  Package :: {
    pname = "hello2",
    version = "0.1",
    files = ["tests/hello2.el"],
    dependencies = ["hello"],
    localDependencies = ["hello"],
    recipe = ''
    (hello2 :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/hello2.el"))
    ''
  }
]
