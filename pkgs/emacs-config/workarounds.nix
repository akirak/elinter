{
  buttercup = _: super: {
    files = builtins.removeAttrs super.files [ "buttercup-pkg.el" ];
  };
}
