let todo = () => {
  exception Todo;
  raise(Todo);
};

let throw = message => {
  exception Throw(string);
  raise(Throw(message));
};
