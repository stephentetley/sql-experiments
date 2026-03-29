
mod TestDuckAST {

    use Assert.{assertTrue}
    use DuckAST.Syntax.{TODO, QueryNodeType}

    @Test
    def test01(): Unit \ {Assert, IO} =
        let x1 = (QueryNodeType.SELECT_NODE, List#{}, TODO.TODO);
        let json = DuckAST.Syntax.toJsonQueryNodeProperties(x1);
        println("${json}");
        assertTrue(true)

}
