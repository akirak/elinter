{
  Package = {
    Type = {
      pname : Text,
      version : Text,
      files : List Text,
      localDependencies : List Text,
      dependencies : List Text,
      mainFile : Optional Text,
      recipe : Text
    },
    default = {
      localDependencies = [] : List Text,
      mainFile = None Text
    }
  }
}

