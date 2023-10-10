package co.blocke.reflect;

import java.util.Optional;

public class JavaOption3<T> {
  public JavaOption3() {}

  private Optional<T> fld;
  public Optional<T> getFld() { return fld; }
  public void setFld(Optional<T> f) { fld = f; }
}