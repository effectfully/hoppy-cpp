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

And it transforms

```
QGraphicsView::FullViewportUpdate	0	When any visible part of the scene changes or is reexposed, QGraphicsView will update the entire viewport. This approach is fastest when QGraphicsView spends more time figuring out what to draw than it would spend drawing (e.g., when very many small items are repeatedly updated). This is the preferred update mode for viewports that do not support partial updates, such as QGLWidget, and for viewports that need to disable scroll optimization.
QGraphicsView::MinimalViewportUpdate	1	QGraphicsView will determine the minimal viewport region that requires a redraw, minimizing the time spent drawing by avoiding a redraw of areas that have not changed. This is QGraphicsView's default mode. Although this approach provides the best performance in general, if there are many small visible changes on the scene, QGraphicsView might end up spending more time finding the minimal approach than it will spend drawing.
QGraphicsView::SmartViewportUpdate	2	QGraphicsView will attempt to find an optimal update mode by analyzing the areas that require a redraw.
QGraphicsView::BoundingRectViewportUpdate	4	The bounding rectangle of all changes in the viewport will be redrawn. This mode has the advantage that QGraphicsView searches only one region for changes, minimizing time spent determining what needs redrawing. The disadvantage is that areas that have not changed also need to be redrawn.
QGraphicsView::NoViewportUpdate	3	QGraphicsView will never update its viewport when the scene changes; the user is expected to control all updates. This mode disables all (potentially slow) item visibility testing in QGraphicsView, and is suitable for scenes that either require a fixed frame rate, or where the viewport is otherwise updated externally.
```

(directly copypasted from the Qt documentation) into the same list.

The application starts in the "method" mode and transforms method headers.
In order to change a mode you can type either "enum" or "method" and press Enter twice (you'll see "OK" then).

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