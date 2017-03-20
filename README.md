# hoppy-cpp

This is a quick and dirty tool that allows to generate approximate [Hoppy](https://gitlab.com/khumba/hoppy) bindings from C++ method headers and enums. Run it (e.g. via `stack build && stack exec hoppy-cpp`), then paste a bunch of bindings like

```cpp
QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsPolygonItem * addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())
qreal height() const
qreal height(int blah) const
void invalidate(qreal x, qreal y, SceneLayers layers = AllLayers)
void invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)
```

press Enter twice and it'll return

```haskell
  , mkMethod "addEllipse" [doubleT, doubleT, doubleT, doubleT] $ ptrT $ objT c_QGraphicsEllipseItem
  , mkMethod' "addEllipse" "addEllipseAll" [doubleT, doubleT, doubleT, doubleT, objT c_QPen, objT c_QBrush] $ ptrT $ objT c_QGraphicsEllipseItem
  , mkMethod "addPolygon" [objT c_QPolygonF] $ ptrT $ objT c_QGraphicsPolygonItem
  , mkMethod' "addPolygon" "addPolygonAll" [objT c_QPolygonF, objT c_QPen, objT c_QBrush] $ ptrT $ objT c_QGraphicsPolygonItem
  , mkConstMethod' "height" "height" [] doubleT
  , mkConstMethod' "height" "height" [intT] doubleT
  , mkMethod' "invalidate" "invalidate" [doubleT, doubleT] voidT
  , mkMethod' "invalidate" "invalidateAll" [doubleT, doubleT, objT c_SceneLayers] voidT
  , mkMethod' "invalidate" "invalidate" [doubleT, doubleT, doubleT, doubleT] voidT
  , mkMethod' "invalidate" "invalidateAll" [doubleT, doubleT, doubleT, doubleT, objT c_SceneLayers] voidT
```

And you can paste again. In the case of enums `hoppy-cpp` transforms

```
FullViewportUpdate 0
MinimalViewportUpdate 1
SmartViewportUpdate 2
BoundingRectViewportUpdate 4
NoViewportUpdate 3
```

into

```haskell
  , (0, ["full","viewport","update"])
  , (1, ["minimal","viewport","update"])
  , (2, ["smart","viewport","update"])
  , (4, ["bounding","rect","viewport","update"])
  , (3, ["no","viewport","update"])
```

Groups of method and enum bindings can be interleaved freely.

## Behavior

- any non-primitive type is treated as a class, so an enum translates into the `objT` and you have to change this manually after bindings are generated
- overloaded methods translate into a group like this:

```haskell
  , mkMethod' "foo" "foo" ...
  , mkMethod' "foo" "foo" ...
  , mkMethod' "foo" "foo" ...
```

and appropriate names have to be provided explicitly

- each method with optional parameters produces two bindings: one with all the parameters being excluded and the other with all the parameters being included:

```haskell
  , mkMethod "foo" ...
  , mkMethod' "foo" "fooAll" ...
```