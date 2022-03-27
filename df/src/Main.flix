use BasicDb.DbMonad;
use DF/DuckCsv.{DuckDbColumn};

def main(args: Array[String]): Int32 & Impure = 
    use Applicative.{product3};
    println("Running...");
    if (Array.length(args) > 0) {
        let csvpath = args[0];
        let action = product3(action1(csvpath), action2(csvpath), action3());
        match BasicDb/DbMonad.runDuckDB(action, System/FilePath.new("e:/coding/flix/sql-experiments/df/data/test1.duckdb")) {
            case Err(e) => println("Error: ${e}")
            case Ok((xs, ys, zs)) => { 
                println("xs...");
                List.foreach(println, xs);
                println("ys...");
                List.foreach(println, ys);
                println("cols...");
                List.foreach(printCol >> println, zs)
            }
        }
    } else println("Supply path to csv...");
    0



def action1(csvpath: String): DbMonad[List[(String, String)]] =
    DF/DuckCsv.queryCsv(csvpath)

def action2(csvpath: String): DbMonad[List[(String, String)]] =
    use BasicDb/DbMonad.{flatMap, point};
    let* _      = DF/DuckCsv.loadCsv(tableName = "datalogs", csvpath);
    let* xs     = DF/DuckCsv.query1(tableName = "datalogs");
    point(xs)

def action3(): DbMonad[List[DuckDbColumn]] =
    DF/DuckCsv.getDuckDbColumns(tableName = "datalogs")

def printCol(r: DuckDbColumn): String = 
    "{columnName=${r.columnName}, ordinalPosition=${r.ordinalPosition}, dataType=${r.dataType}, isNullable=${r.isNullable}}"