type key =
  | Up
  | Down
  | Left
  | Right
  | Space;

let fromString = (key: string): option(key) =>
  switch (key) {
  | "ArrowUp" => Some(Up)
  | "ArrowDown" => Some(Down)
  | "ArrowLeft" => Some(Left)
  | "ArrowRight" => Some(Right)
  | " " => Some(Space)
  | _ => None
  };
