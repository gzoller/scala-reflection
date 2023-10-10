package co.blocke.reflect;

enum Color 
{ 
    RED, GREEN, BLUE; 
}

public class JavaEnum {
  public JavaEnum() {}

  private Color color;
  public Color getColor() { return color; }
  public void setColor(Color n) { color = n; }
}