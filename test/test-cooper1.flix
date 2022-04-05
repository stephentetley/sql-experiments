/// test-cooper1.flix


namespace TestCooper {

use Cooper1/SqlLang.SQLQuery.{SQLSelect};
use Cooper1/SqlLang.SQLSelectFromWhere.{SQLSelectFromWhere};
use Cooper1/SqlLang.SQLTableAlias.{SQLTableAs};
use Cooper1/SqlLang.SQLSelector.{SQLFieldAs};
use Cooper1/SqlLang.SQLExpr.{SQLProj, SQLCst};
use Cooper1/SqlLang.SQLConstant.{SQLInt, SQLString};
use Cooper1/SqlLang.{ppSQLQuery};

@test
def test01(): Unit & Impure = 
    let q1 = SQLSelect(
        SQLSelectFromWhere(
          SQLFieldAs(SQLProj("t1", "first_name"), "name") :: SQLFieldAs(SQLProj("t1", "age_in_years"), "age") :: Nil
        , SQLTableAs("employee", "t1") :: Nil
        , SQLCst(SQLInt(1))      /// Makes valid but vacuous where clause
        )
    );
    let ans = Text/PrettyPrint.render(80, ppSQLQuery(q1));
    Console.printLine(ans)

}    