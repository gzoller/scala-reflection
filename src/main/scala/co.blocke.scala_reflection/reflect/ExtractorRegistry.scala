package co.blocke.scala_reflection
package reflect

import extractors.* 

object ExtractorRegistry:

  lazy val extractors: List[TypeExtractor[_]] = 
    List(
      OptionExtractor(),
      EitherExtractor(),
      SeqExtractor(),
      ScalaArrayExtractor(),
      // MapExtractor(),
      // TupleExtractor(),
      // TryExtractor(),
      // JavaQueueExtractor(),
      // JavaStackExtractor(),
      // JavaSetExtractor(),
      // JavaListExtractor(),
      // OptionalExtractor(),
      // JavaMapExtractor()
    )
