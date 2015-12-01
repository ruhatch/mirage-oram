(**

  Simple non-persistent B-Trees

  I would like to formally apologise for the imperative nature of this implementation, but time is permitting

  Pass in block size on creation

  This determines t (minimum degree) as t = (B + 10) / 36

  -8 off the front for pointers and floor it, so t = |_ (B + 2) / 36 _|

  Thus for 512B sectors, get minimum degree of 16 children

  Need to store t somewhere

  Root node is B-Tree and stores t as well as other things

  Each node is then a Cstruct laid out as

  --------------------------------------------------------------------------------------------------------------------------
  | noKeys n | minDegree t | pageSize p | leaf b | child 1 | key 1 | value 1 | child 2 | ... | key n | value n | child n+1 |
  --------------------------------------------------------------------------------------------------------------------------

  Child is a pointer to another Node of the B-Tree (uint64)
  Key is an inode number based on the hash of the filename (uint16)
  Value is a pointer to the actual inode (uint64)

*)

module type ALLOCATOR = sig

  type t

  type pointer

  val alloc : t -> int -> pointer list

end

module type STORE = sig

  type t

  type 'a io

  type page_aligned_buffer

  type error

  type pointer

  val bind : 'a io -> ('a -> 'b io) -> 'b io

  val return : 'a -> 'a io

  val read : t -> pointer -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

  val write : t -> pointer -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

end

module Make (A : ALLOCATOR) (S : STORE with type pointer = A.pointer) (N : Node.NODE with type t = S.page_aligned_buffer and type pointer = A.pointer) = struct

  let (>>=) = S.bind

  let return = S.return

  type t = {
    allocator : A.t;
    store : S.t;
    mutable root : N.t;
    mutable rootAddress : A.pointer;
    minDegree : int;
  }

  let ( >>= ) x f = x >>= function
    | `Error e -> return (`Error e)
    | `Ok x -> f x

  let create allocator store pageSize =
    let root = N.create pageSize in
    N.setLeaf root true;
    let minDegree = N.minDegree root in
    let [rootAddress] = A.alloc allocator 1 in
    S.write store rootAddress [root] >>= fun () ->
    return (`Ok ({ allocator ; store ; root ; rootAddress ; minDegree }))

  let connect allocator store pageSize rootAddress =
    let root = N.create pageSize in
    S.read store rootAddress [root] >>= fun () ->
    let minDegree = N.minDegree root in
    return (`Ok ({ allocator ; store ; root ; rootAddress ; minDegree }))

  let splitChild t parent parentAddress child childAddress childIndex =
    let newChild = N.create (N.pageSize t.root) in
    let [newChildAddress] = A.alloc t.allocator 1 in
    N.setLeaf newChild (N.leaf child);
    N.setNoKeys newChild (t.minDegree - 1);
    for j = 1 to t.minDegree - 1 do
      N.setKey newChild j (N.getKey child (j + t.minDegree));
      N.setValue newChild j (N.getValue child (j + t.minDegree))
    done;
    if not (N.leaf child)
    then
      for j = childIndex to t.minDegree do
        N.setChild newChild j (N.getChild child (j + t.minDegree))
      done;
    N.setNoKeys child (t.minDegree - 1);
    for j = N.noKeys parent + 1 downto childIndex + 1 do
      N.setChild parent (j + 1) (N.getChild parent j)
    done;
    N.setChild parent (childIndex + 1) newChildAddress;
    for j = N.noKeys parent downto childIndex do
      N.setKey parent (j + 1) (N.getKey parent j);
      N.setValue parent (j + 1) (N.getValue parent j)
    done;
    N.setKey parent childIndex (N.getKey child t.minDegree);
    N.setValue parent childIndex (N.getValue child t.minDegree);
    N.setNoKeys parent (N.noKeys parent + 1);
    S.write t.store parentAddress [parent] >>= fun () ->
    S.write t.store childAddress [child] >>= fun () ->
    S.write t.store newChildAddress [newChild] >>= fun () ->
    return (`Ok (newChild,newChildAddress))

  let rec insertNonfull t node nodeAddress key value =
    Printf.printf "Inserting. Node is leaf? %b" (N.leaf node);
    let i = ref (N.noKeys node) in
    if N.leaf node
    then (
      while !i >= 1 && key < N.getKey node !i do
        N.setKey node (!i + 1) (N.getKey node !i);
        N.setValue node (!i + 1) (N.getValue node !i);
        decr i
      done;
      N.setKey node (!i + 1) key;
      N.setValue node (!i + 1) value;
      N.setNoKeys node (N.noKeys node + 1);
      S.write t.store nodeAddress [node]
    ) else (
      while !i >= 1 && key < N.getKey node !i do
        decr i
      done;
      incr i;
      let child = N.create (N.pageSize t.root) in
      let childAddress = N.getChild node !i in
      S.read t.store childAddress [child] >>= fun () ->
      if N.noKeys child = (2 * t.minDegree - 1)
      then (
        splitChild t node nodeAddress child childAddress !i >>= fun (n,na) ->
        if key > N.getKey node !i
        then insertNonfull t n na key value
        else insertNonfull t child (N.getChild node !i) key value
      ) else insertNonfull t child (N.getChild node !i) key value
    )

  let insert t key value =
    if N.noKeys t.root = (2 * t.minDegree - 1)
    then (
      let s = N.create (N.pageSize t.root) in
      let [sa] = A.alloc t.allocator 1 in
      N.setLeaf s false;
      N.setChild s 1 t.rootAddress;
      splitChild t s sa t.root t.rootAddress 1 >>= fun (n, na) ->
      t.root <- s;
      t.rootAddress <- sa;
      insertNonfull t s sa key value
    ) else insertNonfull t t.root t.rootAddress key value

  let rec find t node key =
    (* let keyStrings = N.printKeys node in
    Printf.printf "Looking for key in list: ";
    List.iter (fun k -> Printf.printf "%s " k) keyStrings;
    Printf.printf "\n"; *)
    let i = ref 1 in
    while !i <= N.noKeys node && key > N.getKey node !i do
      incr i;
    done;
    if !i <= N.noKeys node && key = N.getKey node !i
    then return (`Ok (Some (N.getValue node !i)))
    else if N.leaf node
    then return (`Ok None)
    else (
      let child = N.create (N.pageSize t.root) in
      let childAddress = N.getChild node !i in
      S.read t.store childAddress [child] >>= fun () ->
      find t child key
    )

end
