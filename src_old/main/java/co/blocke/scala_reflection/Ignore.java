package co.blocke.scala_reflection;

import java.lang.annotation.*;

/** Annotation for Java getters or setters to tell reflector to ignore the decorated property for
 *  the purposes of reflection.
 */

@Inherited
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface Ignore {
}