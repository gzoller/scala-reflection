package co.blocke.reflect;

import java.util.Optional;

public class JavaOption2 {
  public JavaOption2() {}

  private Optional<Optional<Integer>> fld;
  public Optional<Optional<Integer>> getFld() { return fld; }
  public void setFld(Optional<Optional<Integer>> f) { fld = f; }
}