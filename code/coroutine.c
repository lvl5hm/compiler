
int subroutine(int *foo) {
  while (*foo < 15) {
    (*foo)++;
    cr_yield(*foo);
  }
  
  return *foo;
}


int routine(int n) {
  for (int i = 0; i < n; i++) {
    yield i;
  }
  
  yield subroutine(&i);
  printf(i);
  
  return -69;
}


int routine(State *state, int n) {
  switch (state->stage) {
    case 0:
    state->i = 0;
    stage->stage++;
    
    case 1:
    if (state->i < n) {
      return state->i++;
    }
    state->stage++;
    
    case 2:
    return subroutine(&state->substate, &state->i);
    state->stage++;
    
    case 3:
    return -69;
  }
}


int main() {
  State *state = cr_start(routine(10));
  cr_advance(routine, state);
}