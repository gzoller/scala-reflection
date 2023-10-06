package co.blocke.reflect;

import java.util.Optional;

public class JavaOption1 {
  public JavaOption1() {}

  private Optional<Integer> fld;
  public Optional<Integer> getFld() { return fld; }
  public void setFld(Optional<Integer> f) { fld = f; }
}