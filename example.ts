import { createCLRPCClient } from './generated/client';
import type { Procedures } from './generated/procedures';

/* Procedure Definitions in Common Lisp
(ql:quickload :clrpc)
(in-package :clrpc)

(define-procedure greeting ((name string))
  (format nil "Hello, ~A!" name))

(start-server :port 3000)

(defclass address ()
  ((street :accessor address-street :initarg :street :type string)
   (city :accessor address-city :initarg :city :type string)))

(defclass person ()
  ((name :accessor person-name :initarg :name :type string)
   (address :accessor person-address :initarg :address :type address)))

(define-procedure say-hello ((person person))
  (format nil "Hello, ~A from ~A!" 
    (person-name person)
    (address-city (person-address person))))
*/

async function main() {
  const clrpc = createCLRPCClient<Procedures>('http://localhost:3000');
  {
    const res = await clrpc.greeting.query({
      name: "John"
    });
    console.log(res);
  }

  {
    const res = await clrpc.say_hello.query({
      person: {
        name: "John",
        address: {
          street: "123 Main St",
          city: "Boston",
        }
      }
    });
    console.log(res);
  }
}

main();
