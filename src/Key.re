type position = {
  x: int,
  y: int,
};

let modifyX = (position, f) => {...position, x: f(position.x)};

let modifyY = (position, f) => {...position, y: f(position.y)};

type direction =
  | Up
  | Down
  | Left
  | Right;

let move = (direction: direction, position: position): position =>
  switch (direction) {
  | Up => position->modifyY(y => y + 1)
  | Down => position->modifyY(y => y - 1)
  | Left => position->modifyX(_, x => x - 1)
  | Right => position->modifyX(_, x => x + 1)
  };

let revert = (direction: direction) =>
  switch (direction) {
  | Up => Down
  | Down => Up
  | Left => Right
  | Right => Left
  };

type key =
  | Direction(direction)
  | Space;

let direction = x => Direction(x);

let fromString = (key: string): option(key) =>
  switch (key) {
  | "ArrowUp" => Some(Direction(Up))
  | "ArrowDown" => Some(Direction(Down))
  | "ArrowLeft" => Some(Direction(Left))
  | "ArrowRight" => Some(Direction(Right))
  | " " => Some(Space)
  | _ => None
  };
