// Copyright 2020 The Druid Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! A stack based tooltip widget.

use std::{cell::RefCell, fmt::Debug, rc::Rc, sync::Arc};

use crate::{animation::AnimationCurve, Stack, StackChildParams, StackChildPosition};
use druid::{
    text::RichText,
    widget::{
        DefaultScopePolicy, Either, Label, LensScopeTransfer, Scope, SizedBox, WidgetWrapper,
    },
    Color, Data, KeyOrValue, Lens, Point, RenderContext, Selector, SingleUse, Size, Widget,
    WidgetExt, WidgetId, WidgetPod,
};

const FORWARD: Selector<SingleUse<(WidgetId, Point)>> = Selector::new("tooltip.forward");
const POINT_UPDATED: Selector = Selector::new("tooltip.label.point_updated");
pub(crate) const ADVISE_TOOLTIP_SHOW: Selector<Point> =
    Selector::new("tooltip.advise_show_tooltip");
pub(crate) const CANCEL_TOOLTIP_SHOW: Selector = Selector::new("tooltip.cancel_show_tooltip");

type StackTooltipActual<T> = Scope<
    DefaultScopePolicy<
        fn(T) -> TooltipState<T>,
        LensScopeTransfer<tooltip_state_derived_lenses::data<T>, T, TooltipState<T>>,
    >,
    StackTooltipInternal<T>,
>;

pub struct StackTooltip<T: Data>(StackTooltipActual<T>);

impl<T: Data> StackTooltip<T> {
    pub fn new<W: Widget<T> + 'static>(widget: W, label: impl Into<PlainOrRich>) -> Self {
        Self(StackTooltipInternal::new(widget, label))
    }

    pub fn custom<W: Widget<T> + 'static, WL: Widget<T> + 'static>(widget: W, label: WL) -> Self {
        Self(StackTooltipInternal::custom(widget, label))
    }

    pub fn set_background_color(&mut self, color: impl Into<KeyOrValue<Color>>) {
        self.0.wrapped_mut().set_background_color(color)
    }

    pub fn with_background_color(mut self, color: impl Into<KeyOrValue<Color>>) -> Self {
        self.set_background_color(color);

        self
    }

    pub fn set_border_width(&mut self, width: f64) {
        self.0.wrapped_mut().set_border_width(width)
    }

    pub fn with_border_width(mut self, width: f64) -> Self {
        self.set_border_width(width);

        self
    }

    pub fn set_border_color(&mut self, color: impl Into<KeyOrValue<Color>>) {
        self.0.wrapped_mut().set_border_color(color);
    }

    pub fn with_border_color(mut self, color: impl Into<KeyOrValue<Color>>) -> Self {
        self.set_border_color(color);

        self
    }

    pub fn set_crosshair(&mut self, crosshair: bool) {
        self.0.wrapped_mut().set_crosshair(crosshair)
    }

    pub fn with_crosshair(mut self, crosshair: bool) -> Self {
        self.set_crosshair(crosshair);

        self
    }

    pub fn set_offset(&mut self, offset: impl Into<Point>) {
        self.0.wrapped_mut().set_offset(offset)
    }

    pub fn with_offset(mut self, offset: impl Into<Point>) -> Self {
        self.set_offset(offset);

        self
    }

    pub fn set_duration(&mut self, duration: f64) {
        self.0.wrapped_mut().set_duration(duration)
    }

    pub fn with_duration(mut self, duration: f64) -> Self {
        self.set_duration(duration);

        self
    }

    pub fn set_animation_curve(&mut self, animation: AnimationCurve) {
        self.0.wrapped_mut().set_animation_curve(animation)
    }

    pub fn with_animation_curve(mut self, animation: AnimationCurve) -> Self {
        self.set_animation_curve(animation);

        self
    }
}

impl<T: Data> Widget<T> for StackTooltip<T> {
    fn event(
        &mut self,
        ctx: &mut druid::EventCtx,
        event: &druid::Event,
        data: &mut T,
        env: &druid::Env,
    ) {
        self.0.event(ctx, event, data, env)
    }

    fn lifecycle(
        &mut self,
        ctx: &mut druid::LifeCycleCtx,
        event: &druid::LifeCycle,
        data: &T,
        env: &druid::Env,
    ) {
        self.0.lifecycle(ctx, event, data, env)
    }

    fn update(&mut self, ctx: &mut druid::UpdateCtx, old_data: &T, data: &T, env: &druid::Env) {
        self.0.update(ctx, old_data, data, env)
    }

    fn layout(
        &mut self,
        ctx: &mut druid::LayoutCtx,
        bc: &druid::BoxConstraints,
        data: &T,
        env: &druid::Env,
    ) -> Size {
        self.0.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut druid::PaintCtx, data: &T, env: &druid::Env) {
        self.0.paint(ctx, data, env)
    }
}

#[derive(Clone, Data, Lens)]
struct TooltipState<T> {
    data: T,
    show: bool,
    position: StackChildPosition,
    label_size: Option<Size>,
    offset: Point,
}

impl<T> Debug for TooltipState<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TooltipState")
            .field("show", &self.show)
            .field("position", &self.position)
            .field("label_size", &self.label_size)
            .field("offset", &self.offset)
            .finish()
    }
}

type BackgroundCell = Rc<RefCell<Option<KeyOrValue<Color>>>>;
type BorderCell = Rc<RefCell<(Option<KeyOrValue<Color>>, Option<f64>)>>;

struct StackTooltipInternal<T> {
    widget: WidgetPod<TooltipState<T>, Stack<TooltipState<T>>>,
    label_id: Option<WidgetId>,
    background: BackgroundCell,
    border: BorderCell,
    use_crosshair: bool,
    offset: Option<Point>,
    duration: f64,
    animation_curve: Option<AnimationCurve>,
}

fn make_state<T: Data>(data: T) -> TooltipState<T> {
    TooltipState {
        data,
        show: false,
        position: StackChildPosition::new().height(Some(0.0)),
        label_size: None,
        offset: Point::default(),
    }
}

impl<T: Data> StackTooltipInternal<T> {
    fn new<W: Widget<T> + 'static>(
        widget: W,
        label: impl Into<PlainOrRich>,
    ) -> StackTooltipActual<T> {
        let rich_text = match label.into() {
            PlainOrRich::Plain(plain) => RichText::new(plain.into()),
            PlainOrRich::Rich(rich) => rich,
        };

        Self::custom(widget, Label::raw().lens(druid::lens::Constant(rich_text)))
    }

    fn custom<W: Widget<T> + 'static>(
        widget: W,
        label: impl Widget<T> + 'static,
    ) -> StackTooltipActual<T> {
        let background = BackgroundCell::default();
        let border = BorderCell::default();
        let label_id = WidgetId::next();
        let stack = Stack::new()
            .with_child(widget.lens(TooltipState::data))
            .with_positioned_child(
                Either::new(
                    |state: &TooltipState<T>, _| state.show && is_some_position(&state.position),
                    TooltipLabel::custom(label, label_id, background.clone(), border.clone()),
                    SizedBox::empty(),
                ),
                StackChildParams::dynamic(|TooltipState { position, .. }: &TooltipState<T>, _| {
                    position
                })
                .duration(0.0),
            );

        Scope::from_lens(
            make_state as fn(T) -> TooltipState<T>,
            TooltipState::data,
            Self {
                widget: WidgetPod::new(stack),
                label_id: Some(label_id),
                background,
                border,
                use_crosshair: false,
                offset: None,
                duration: 0.0,
                animation_curve: None,
            },
        )
    }

    pub fn set_background_color(&mut self, color: impl Into<KeyOrValue<Color>>) {
        self.background.borrow_mut().replace(color.into());
    }

    pub fn set_border_width(&mut self, width: f64) {
        self.border.borrow_mut().1.replace(width);
    }

    pub fn set_border_color(&mut self, color: impl Into<KeyOrValue<Color>>) {
        self.border.borrow_mut().0.replace(color.into());
    }

    pub fn set_crosshair(&mut self, crosshair: bool) {
        self.use_crosshair = crosshair
    }

    pub fn set_offset(&mut self, offset: impl Into<Point>) {
        self.offset = Some(offset.into());
    }

    pub fn set_duration(&mut self, duration: f64) {
        self.duration = duration
    }

    pub fn set_animation_curve(&mut self, animation: AnimationCurve) {
        self.animation_curve = Some(animation)
    }
}

impl<T: Data> Widget<TooltipState<T>> for StackTooltipInternal<T> {
    fn event(
        &mut self,
        ctx: &mut druid::EventCtx,
        event: &druid::Event,
        data: &mut TooltipState<T>,
        env: &druid::Env,
    ) {
        if let Some(pos) = if let druid::Event::MouseMove(mouse) = event {
            Some(mouse.pos)
        } else if let druid::Event::Command(cmd) = event {
            cmd.get(FORWARD)
                .and_then(SingleUse::take)
                .and_then(|(id, point)| {
                    self.label_id
                        .filter(|label_id| label_id == &id)
                        .and(Some(point))
                })
                .map(|point| (point - ctx.window_origin()).to_point())
        } else {
            None
        } {
            if ctx.is_hot() && ctx.size().to_rect().contains(pos) {
                let offset = self.offset.unwrap_or_default();
                data.offset = offset;

                let mut x = pos.x;
                let mut y = pos.y;

                if let Some(size) = data.label_size {
                    if x + size.width + ctx.window_origin().x
                        > ctx.window().get_size().width - ctx.window().content_insets().x_value()
                    {
                        x -= size.width
                    };
                    if y + size.height + ctx.window_origin().y
                        > ctx.window().get_size().height - ctx.window().content_insets().y_value()
                    {
                        y -= size.height
                    };
                }

                data.position = StackChildPosition::new()
                    .left(Some(x))
                    .top(Some(y))
                    .height(None);

                data.show = true;

                if self.use_crosshair {
                    ctx.set_cursor(&druid::Cursor::Crosshair);
                }

                if let Some(label_id) = self.label_id {
                    if data.label_size.is_none() {
                        ctx.submit_command(POINT_UPDATED.to(label_id));
                    }
                    ctx.submit_command(ADVISE_TOOLTIP_SHOW.with(ctx.to_window(pos)));
                }
            } else {
                reset_position(&mut data.position);
                data.position.height = Some(0.0);
                data.show = false;
            }

            if let druid::Event::Command(_) = event {
                return;
            }
        } else if let druid::Event::Notification(notif) = event {
            if notif.is(CANCEL_TOOLTIP_SHOW) && notif.route() == self.widget.id() {
                reset_position(&mut data.position);
                data.position.height = Some(0.0);
                data.show = false;

                ctx.set_handled();
            }
        };

        self.widget.event(ctx, event, data, env)
    }

    fn lifecycle(
        &mut self,
        ctx: &mut druid::LifeCycleCtx,
        event: &druid::LifeCycle,
        data: &TooltipState<T>,
        env: &druid::Env,
    ) {
        self.widget.lifecycle(ctx, event, data, env)
    }

    fn update(
        &mut self,
        ctx: &mut druid::UpdateCtx,
        _old_data: &TooltipState<T>,
        data: &TooltipState<T>,
        env: &druid::Env,
    ) {
        self.widget.update(ctx, data, env)
    }

    fn layout(
        &mut self,
        ctx: &mut druid::LayoutCtx,
        bc: &druid::BoxConstraints,
        data: &TooltipState<T>,
        env: &druid::Env,
    ) -> druid::Size {
        self.widget.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut druid::PaintCtx, data: &TooltipState<T>, env: &druid::Env) {
        self.widget.paint(ctx, data, env)
    }
}

struct TooltipLabel<T, W> {
    id: WidgetId,
    label: Rc<RefCell<WidgetPod<T, W>>>,
    background: BackgroundCell,
    border: BorderCell,
}

impl<T: Data, W: Widget<T>> TooltipLabel<T, W> {
    pub fn custom(label: W, id: WidgetId, background: BackgroundCell, border: BorderCell) -> Self {
        let label = WidgetPod::new(label);

        Self {
            id,
            label: RefCell::new(label).into(),
            background,
            border,
        }
    }
}

impl<T: Data, W: Widget<T> + 'static> Widget<TooltipState<T>> for TooltipLabel<T, W> {
    fn event(
        &mut self,
        ctx: &mut druid::EventCtx,
        event: &druid::Event,
        data: &mut TooltipState<T>,
        env: &druid::Env,
    ) {
        if let druid::Event::MouseMove(mouse) = event {
            ctx.submit_command(FORWARD.with(SingleUse::new((ctx.widget_id(), mouse.window_pos))))
        } else if let druid::Event::Command(cmd) = event {
            if cmd.is(POINT_UPDATED) {
                if let Some(left) = data.position.left {
                    let label_width = ctx.size().width;
                    if left + label_width + ctx.window_origin().x > ctx.window().get_size().width {
                        data.position.left.replace(left - label_width);
                    }
                }
                if let Some(top) = data.position.top {
                    let label_height = ctx.size().height;
                    if top + label_height + ctx.window_origin().y > ctx.window().get_size().height {
                        data.position.top.replace(top - label_height);
                    }
                }

                if !ctx.size().is_empty() {
                    data.label_size.replace(ctx.size());
                }

                ctx.request_paint();
            }
        }

        self.label
            .borrow_mut()
            .event(ctx, event, &mut data.data, env)
    }

    fn lifecycle(
        &mut self,
        ctx: &mut druid::LifeCycleCtx,
        event: &druid::LifeCycle,
        data: &TooltipState<T>,
        env: &druid::Env,
    ) {
        self.label
            .borrow_mut()
            .lifecycle(ctx, event, &data.data, env)
    }

    fn update(
        &mut self,
        ctx: &mut druid::UpdateCtx,
        _old_data: &TooltipState<T>,
        data: &TooltipState<T>,
        env: &druid::Env,
    ) {
        self.label.borrow_mut().update(ctx, &data.data, env)
    }

    fn layout(
        &mut self,
        ctx: &mut druid::LayoutCtx,
        bc: &druid::BoxConstraints,
        data: &TooltipState<T>,
        env: &druid::Env,
    ) -> druid::Size {
        let mut size = self.label.borrow_mut().layout(ctx, bc, &data.data, env);

        self.label.borrow_mut().set_origin(ctx, data.offset);

        size.width += data.offset.x;
        size.height += data.offset.y;
        size
    }

    fn paint(&mut self, ctx: &mut druid::PaintCtx, data: &TooltipState<T>, env: &druid::Env) {
        let mut rect = ctx
            .size()
            .to_rect()
            .inset((-data.offset.x, -data.offset.y, 0.0, 0.0));
        rect.x0 -= 2.0;
        rect.y1 += 2.0;

        let fill_brush = ctx.solid_brush(
            if let Some(background) = self.background.borrow().as_ref() {
                background.resolve(env)
            } else {
                env.get(druid::theme::BACKGROUND_DARK)
            },
        );
        let border_brush = ctx.solid_brush(if let Some(border) = self.border.borrow().0.as_ref() {
            border.resolve(env)
        } else {
            env.get(druid::theme::BORDER_DARK)
        });
        let border_width = if let Some(width) = self.border.borrow().1.as_ref() {
            *width
        } else {
            env.get(druid::theme::TEXTBOX_BORDER_WIDTH)
        };

        let label = self.label.clone();
        let data = data.data.clone();
        let env = env.clone();
        ctx.paint_with_z_index(1_000_000, move |ctx| {
            ctx.fill(rect, &fill_brush);

            label.borrow_mut().paint_always(ctx, &data, &env);

            ctx.stroke(rect, &border_brush, border_width);
        });
    }

    fn id(&self) -> Option<WidgetId> {
        Some(self.id)
    }
}

fn is_some_position(position: &StackChildPosition) -> bool {
    position.top.is_some()
        || position.bottom.is_some()
        || position.left.is_some()
        || position.right.is_some()
}

fn reset_position(position: &mut StackChildPosition) {
    position.top = None;
    position.bottom = None;
    position.left = None;
    position.right = None;
    position.width = None;
    position.height = None;
}

pub enum PlainOrRich {
    Plain(String),
    Rich(RichText),
}

impl From<String> for PlainOrRich {
    fn from(plain: String) -> Self {
        PlainOrRich::Plain(plain)
    }
}

impl From<&str> for PlainOrRich {
    fn from(plain: &str) -> Self {
        PlainOrRich::Plain(plain.to_owned())
    }
}

impl From<Arc<str>> for PlainOrRich {
    fn from(plain: Arc<str>) -> Self {
        PlainOrRich::Plain(plain.to_string())
    }
}

impl From<RichText> for PlainOrRich {
    fn from(rich: RichText) -> Self {
        PlainOrRich::Rich(rich)
    }
}
