package co.blocke.reflect;

public class Person extends co.blocke.scala_reflection.SJCaptureJava {
  public Person() {}

  private String name;
  public String getName() { return name; }
  public void setName(String n) { name = n; }

  private int age;
  public int getAge() { return age; }
  public void setAge(int n) { age = n; }

  private int other;
  public int getOther() { return other; }
  public void setOther(int n) { other = n; }
}
