{ gnu-elpa, melpa }:
[
  {
    type = "elpa";
    path = gnu-elpa.outPath + "/elpa-packages";
  }
  {
    type = "melpa";
    path = melpa.outPath + "/recipes";
  }
]
