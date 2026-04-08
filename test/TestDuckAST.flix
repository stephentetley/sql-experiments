
mod TestDuckAST {

    use Assert.{assertTrue}

    use InteropJson.ReadJson
    use InteropJson.ToJsonValue
    use InteropJson.BuildJsonArray.BuildJsonArray
    use InteropJson.BuildJsonObject.BuildJsonObject

    use DuckAST.Syntax.{QueryNodeType, QueryNodeProperties, CommonTableExpressionMap}
    

    @Test
    def test01(): Unit \ {Assert, IO} =
        let x1 = QueryNodeProperties.QueryNodeProperties(QueryNodeType.SELECT_NODE, 
                                                            List.empty(), 
                                                            Map.empty());
        let json = ToJsonValue.toJsonValue(x1);
        println("${json}");
        assertTrue(true)

    @Test
    def test02(): Unit \ {Assert, ReadJson, IO} = 
        let json = InteropJson.ReadJson.readJson("./data/select_2.json");
        println("----------");
        println("${json}");
        assertTrue(true)

    @Test
    def test03(): Unit \ {Assert, IO} = 
        let json = DuckAST.Temp.ast01() |> InteropJson.ToJsonValue.toJsonValue;
        println("----------");
        println("${json}");
        assertTrue(true)


    // @Test
    // def testJson01(): Unit \ {Assert, IO} =
    //     let rarr = run {
    //         BuildJsonArray.addInt32(12);
    //         BuildJsonArray.addString("world");
    //         BuildJsonArray.addBigInt(1000ii);
    //         BuildJsonArray.addFloat64(1.0763f64);
    //         true
    //     } with InteropJson.BuildJsonArray.evalBuildJsonValue;
    //     println("${rarr}");
    //     match rarr {
    //         case Err(msg) => println("${msg}")
    //         case Ok(value) => match InteropJson.JsonValue.getJsonArrayElements(value) {
    //                 case Some(xs)   => List.forEach(println, xs)
    //                 case None       => println("None")
    //         }
    //     };
    //     assertTrue(true)

    // @Test
    // def testJson02(): Unit \ {Assert, IO} =
    //     let robj = run {
    //         BuildJsonObject.addInt32(name = "one", 12);
    //         BuildJsonObject.addString(name = "two", "world");
    //         BuildJsonObject.addBigInt(name = "three", 1000ii);
    //         BuildJsonObject.addFloat64(name = "four",1.0763f64);
    //         true
    //     } with InteropJson.BuildJsonObject.evalBuildJsonValue;
    //     println("${robj}");
    //     match robj {
    //         case Err(msg) => println("${msg}")
    //         case Ok(value) => match DuckAST.JsonExtra.getJsonObjectEntries(value) {
    //                 case Some(xs)   => List.forEach(println, xs)
    //                 case None       => println("None")
    //         }
    //     };
    //     assertTrue(true)
    

}
