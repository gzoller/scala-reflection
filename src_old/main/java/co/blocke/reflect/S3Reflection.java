package co.blocke.scala_reflection;

import java.lang.annotation.*;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface S3Reflection {
  String rtype();  // serialized RType object
}