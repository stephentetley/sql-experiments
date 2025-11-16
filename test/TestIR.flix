
mod TestIR {

    use IR.DuckIR.Relation.{Select}
    use IR.DuckIR.Expression.{Constant, SetAlias, StarExpression}
    use IR.DuckIR.Constant.{ConstantVarchar}

    @test
    def test01(): Bool \ IO = 
        let rel1 = Select(List#{SetAlias(Constant(ConstantVarchar("hello")), "message")}, None);
        let doc = Text.Pretty.pPrint(rel1);
        println(ToString.toString(doc));
        true

    @test
    def test02(): Bool \ IO = 
        let rel1 = Select(List#{SetAlias(Constant(ConstantVarchar("hello")), "message")}, None);

        let rel2 = Select(List#{StarExpression(Nil)}, Some(rel1));
        let doc = Text.Pretty.pPrint(rel2);
        println(ToString.toString(doc));
        true

        
}