package co.blocke.reflect;

import java.lang.annotation.*;

/** Annotation for Java getters or setters to tell reflector to ignore the decorated property for
 *  the purposes of reflection.
 */

@Inherited
@Target({ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Ignore {
}