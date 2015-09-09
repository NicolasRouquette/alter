# Alter

Alter allows you to extract differences between algebraic data tyepes

### Example

```scala
case class Person(id: Int, name: String, roleIds: Seq[Int])

val karel = Person(1, "Karel", List(1,2,3))
val dirk = Person(2, "Dirk", List(2,3,4,5))

println(Diff[Person].diff(karel, dirk))
```

Output
```
Changes(
  List(
    Named(id,Update(2)),
    Named(name,Update(Dirk)),
    Named(roleIds,Changes(List(Delete(1), Copy(2), Copy(3), Insert(4), Insert(5))))
  )
)
```

This is useful for

- VirtualDOM
- Database updates (but expensive)
- JSON diff
- Other tree diffs