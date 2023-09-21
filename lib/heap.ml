(* Heap polymorphism *)
(* min heap need 
  if t1 > t2 then (compare t1 t2) > 0
  if t1 = t2 then (compare t1 t2) = 0
  if t1 < t2 then (compare t1 t2) < 0 
  otherwise, if need max heap, 
  if t1 > t2 then (compare t1 t2) < 0
  if t1 = t2 then (compare t1 t2) = 0
  if t1 < t2 then (compare t1 t2) > 0  *)
module type OrderType = sig 
  type t
  val compare : t -> t -> int
end

module type Heap = sig
  type key
  type t
  val create : unit -> t
  val size : t -> int
  val capacity : t -> int
  val is_empty : t -> bool
  val pop : t -> key option
  val push : key -> t -> unit
  val of_list : key list -> t
  val to_list : t -> key list 
  val is_order : key list -> bool
end
    
(* : (Heap with type key = Ord.t ) *)
module Make (Ord : OrderType) : (Heap with type key = Ord.t)   = struct
  type key = Ord.t

  (* 
      size means how many elts in the heap.
      capacity means the maximum amount of elts
      if size will be bigger than capacity, resize will be triggered. 
      complete binary tree is implemented with array.
      Attention : index form 1, 
      so size is the last key's index
      so size must be lower than capcity
      eg. 
      if 1 2 3 is in the heap, size is 3, 
      the array contains index 0 1 2 3, capacity is 3 (len arrary - 1)
      when push 4 into heap, resize must be trigger, 
        doubule capacity *)
  type t = {
    mutable size : int;
    mutable capacity : int;
    mutable bintree :  key option array;
  }

  let compare x y = 
    match x, y with
    | None, _ | _, None -> failwith "uncomparable"
    | Some a, Some b -> Ord.compare a b 

  let create () = {
    size = 0;
    capacity = 3;
    bintree = Array.make 4 None 
  }

  let is_empty heap = 
    heap.size  = 0

  let size heap = heap.size
  let capacity heap = heap.capacity 
  (* enlarge the arr *)
  let resize heap = 
    let old_arr = heap.bintree in 
    let old_capacity = heap.capacity in 
    let new_capacity = (old_capacity + 1) * 2 - 1 in 
    let new_arr =   Array.init ((old_capacity + 1) * 2) 
        (fun i -> if i <= old_capacity then old_arr.(i) else None)
    in
    heap.capacity <- new_capacity;
    heap.bintree <- new_arr
  
  let swap heap i j  = 
    let heap = heap.bintree in
    let tmp = heap.(i) in
    heap.(i) <- heap.(j);
    heap.(j) <- tmp

  let up heap i = 
    let arrheap = heap.bintree in
    let i = ref i in 
    while (!i / 2 <> 0 && compare arrheap.(!i) arrheap.(!i/2) < 0) do
      swap heap !i (!i / 2);
      i := (!i / 2)
    done

  (* if i heavier than children then i will go down *)
  let rec down heap i = 
    let arrheap = heap.bintree in
    let t = ref i in
    if (i * 2 <= heap.size && compare arrheap.(i) arrheap.(i * 2) > 0) 
      then t := i * 2;
    if (i * 2 + 1 <= heap.size && compare arrheap.(!t) arrheap.(i * 2 + 1) > 0 )
      then t := i * 2 + 1;
    if (!t <> i) then begin 
      swap heap i !t;
      down heap !t 
    end

  let pop heap = 
    let arr = heap.bintree in
    let top = arr.(1) in
    if heap.size >= 1 then begin
      swap heap 1 heap.size;
      heap.size <- heap.size - 1;
      down heap 1;
      top
      end
    else
      None

  let push e heap = 
    let last = heap.size + 1 in
    if last > heap.capacity then resize heap;
    heap.bintree.(last) <- Some e;
    heap.size <- last;
    up heap last

  let of_list lst = 
    let heap = create () in
    List.iter (fun e -> push e heap) lst;
    heap

  let rec to_lst_aux acc heap = 
    let top = pop heap in
    match top with
    | None -> List.rev acc
    | Some x -> to_lst_aux (x :: acc) heap 
  let to_list heap = to_lst_aux [] heap
  let rec is_order = function
  | [] | [_] -> true
  | h1 :: (h2 :: t' as t) -> 
      if Ord.compare h1 h2  > 0 then false else is_order t 
  
end
    
    