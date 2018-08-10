function vm() {
let pc = 0;
const stack = [];

const mem = {
  'y/hand': [{'w': 19},{'w': 20},{'w':23}],
  'y/bench': [0],
};

const program = [
  'LVAL', 'y/hand',
  'LEN',
  'JZ', 24,
  'LVAL', 'y/bench',
  'LVAL', 'y/hand',
  'POP',
  'DUP',
  'GET', 'w',
  'CONST', 10,
  'ADD',
  'SET', 'w',
  'PUSH',
  'BURN',
  'CONST', 0,
  'JZ', 0,
  'CONST', -1
];

const program2 = [
  'LVAL', 'y/bench',
  'LVAL', 'y/hand',
  'LEN',
  'JZ', 18,
  'CONST', 10,
  'LVAL', 'y/hand',
  'POP',
  'MUL',
  'PUSH',
  'CONST', 0,
  'JZ', 2,
  'CONST', 20000 
];


let ticks = 0;
while (pc < program.length && ticks < 100 ) {
  const inst = program[pc];
  console.log('\n','PC =',pc,'->',inst);

  let a,b;
 
  switch (inst) {
    case 'CONST':
      stack.push(program[pc+1]);
      pc += 2;
      break;

    case 'ADD':
      b = stack.pop();
      a = stack.pop();
      stack.push(a+b);
      pc++;
    break;


    case 'MUL':
      b = stack.pop();
      a = stack.pop();
      stack.push(a*b);
      pc++;
    break;

    case 'JZ':
      a = stack.pop();
      if (!a) {
        pc = program[pc+1];
      } else {
        pc += 2;
      }
    break;

    case 'LVAL':
      stack.push(mem[program[pc+1]]);
      pc += 2;
    break;

    case 'GET':
      a = stack.pop();
      stack.push(a[program[pc+1]]);
      pc += 2;
    break;

    case 'SET':
      b = stack.pop();
      a = stack.pop();
      a[program[pc+1]] = b;
      stack.push(a);
      pc += 2;
    break;

    case 'DUP':
        a = stack.pop();
        stack.push(a);
        stack.push(a);
        pc++;
        break;


    case 'POP':
      a = stack.pop();
      let tv = a.pop();
      stack.push(tv);
      pc++;
    break;
    
    case 'PUSH':
      b = stack.pop();
      a = stack.pop();
      a.push(b);
      stack.push(a);
      pc++
      break;

    case 'LEN':
      a = stack.pop();
      stack.push(a.length);
      pc++
      break;

    case 'BURN':
      stack.pop();
      pc++;
      break;

    default:
      console.log('INVALID OPCODE', inst);
      return;
  }


      console.log(stack);

      ticks++;

}

  //console.log(mem);
}

/**
 * LVAL
 * GET
 * SET
 * DUP
 * PUSH
 * POP
 * LEN
 * JZ
 * CONST
 * ADD
 * MUL
 *
 * XCHG?
 */

  vm();
