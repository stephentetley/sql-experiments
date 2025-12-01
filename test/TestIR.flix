
mod TestIR {

    use IR.DuckIR.Relation.{Project1, Project, Table, Filter, ReadCSV}
    use IR.DuckIR.Expression.{Literal, ColumnExpression, ComparisonExpression, 
        SetAlias, StarExpression}
    use IR.DuckIR.Literal.{Varchar, Integer}
    use IR.DuckIR.ComparisonOperator.{Equal, GreaterThan}

    @test
    def test01(): Bool \ IO = 
        let rel1 = Project1(List#{SetAlias(Literal(Varchar("hello")), "message")});
        let doc = Text.Pretty.pPrint(rel1);
        println(ToString.toString(doc));
        true

    @test
    def test02(): Bool \ IO = 
        let rel1 = Project1(List#{SetAlias(Literal(Varchar("hello")), "message")});

        let rel2 = Project(List#{StarExpression(Nil, Nil, Nil, Nil)}, rel1);
        let doc = Text.Pretty.pPrint(rel2);
        println(ToString.toString(doc));
        true


    @test
    def test03(): Bool \ IO = 
        let rel1 = Table("main.customers");

        let rel2 = Project(List#{StarExpression(Nil, Nil, Nil, Nil)}, rel1);
        let doc = Text.Pretty.pPrint(rel2);
        println(ToString.toString(doc));
        true


    @test
    def test04(): Bool \ IO = 
        let rel1 = ReadCSV("langs_202511161528.csv");
        let expr1 = ComparisonExpression(GreaterThan, ColumnExpression("name"), Literal(Integer(2000)));
        let rel2 = Filter(Project(List#{StarExpression(Nil, Nil, Nil, Nil)}, rel1), expr1);
        let doc = Text.Pretty.pPrint(rel2);
        println(ToString.toString(doc));
        true


}