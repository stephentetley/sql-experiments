
mod TestDuckAST {

    use Assert.{assertTrue}
    use InteropJson.ReadJson
    use DuckAST.Syntax.{TODO, QueryNodeType}
    use InteropJson.BuildJsonArray.BuildJsonArray

    @Test
    def test01(): Unit \ {Assert, IO} =
        let x1 = (QueryNodeType.SELECT_NODE, List#{}, TODO.TODO);
        let json = DuckAST.Syntax.toJsonQueryNodeProperties(x1);
        println("${json}");
        assertTrue(true)

    @Test
    def test02(): Unit \ {Assert, ReadJson, IO} = 
        let json = InteropJson.ReadJson.readJson("./data/select_2.json");
        println("${json}");
        assertTrue(true)

    @Test
    def test03(): Unit \ {Assert, IO} =
        let rarr = run {
            BuildJsonArray.addInt32(12);
            BuildJsonArray.addString("world");
            BuildJsonArray.addBigInt(1000ii);
            BuildJsonArray.addFloat64(1.0763f64);
            true
        } with InteropJson.BuildJsonArray.evalBuildJsonValue;
        println("${rarr}");
        match rarr {
            case Err(msg) => println("${msg}")
            case Ok(value) => match InteropJson.JsonValue.getJsonArrayElements(value) {
                    case Some(xs)   => List.forEach(println, xs)
                    case None       => println("None")
            }
        };
        assertTrue(true)
    

}
