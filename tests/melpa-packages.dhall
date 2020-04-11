[
  {
    pname = "hello",
    version = "0.1",
    files = ["tests/hello.el", "tests/hello-util.el"],
    localDependencies = [] : List Text,
    dependencies = ["dash"],
    mainFile = Some "tests/hello.el",
    recipe = ''
    (hello :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/hello.el" "tests/hello-util.el"))
    ''
  },
  {
    pname = "hello2",
    version = "0.1",
    files = ["tests/hello2.el"],
    dependencies = ["hello"],
    localDependencies = ["hello"],
    mainFile = None Text,
    recipe = ''
    (hello2 :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/hello2.el"))
    ''
  }
]
