import Test.DocTest
main
  = doctest
      [ "-isrc"
      , "src/Data/Generics/Product.hs"
      , "src/Data/Generics/Sum.hs"
      ]
