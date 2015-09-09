# Alter

Alter allows you to extract differences between algebraic data tyepes

### Example

```scala
case class Person(id: Int, name: String, roleIds: Seq[Int])

object TestApp  extends App {

  val karel = Person(1, "Karel", List(1,2,3))
  val dirk = Person(2, "Dirk", List(3,4,5))

  println(Diff[Person].diff(karel, dirk))
}
```

Output
```
Changes(List(Named(id,Update(2)), Named(name,Update(Dirk)), Named(roleIds,Changes(List(Replace(1,3), Replace(2,4), Replace(3,5))))))
```

This is useful for

- VirtualDOM
- Database updates (but expensive)
- JSON diff
- Other tree diffs