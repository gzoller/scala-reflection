package co.blocke.scala_reflection
package reflect

import extractors.*

object ExtractorRegistry:

  lazy val extractors: List[TypeExtractor[?]] =
    List(
      OptionExtractor(),
      EitherExtractor(),
      SeqExtractor(),
      ArrayExtractor(),
      MapExtractor(),
      TupleExtractor(),
      TryExtractor(),
      JavaCollectionExtractor(),
      JavaOptionalExtractor(),
      JavaMapExtractor()
    )
