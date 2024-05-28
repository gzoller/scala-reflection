package co.blocke.scala_reflection
package reflect

import extractors.*

object ExtractorRegistry:

  lazy val extractors: List[TypeExtractor[?]] =
    List(
      OptionExtractor(),
      EitherExtractor(),
      MapExtractor(),
      SeqExtractor(),
      ArrayExtractor(),
      TupleExtractor(),
      TryExtractor(),
      JavaCollectionExtractor(),
      JavaOptionalExtractor(),
      JavaMapExtractor()
    )
