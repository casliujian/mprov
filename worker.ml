open Printf
open Modul

module type Work_paras = 
sig 
	type t
	val ncores : int 
end;;

module Make(WP: Work_paras) = 
struct 
	type message = 
		| Terminate
		| New_element of WP.t
	let result = ref None
	let ncores = WP.ncores
	let mutex_aray = Array.init ncores (fun () -> Mutex.create ()) 
	let condition_aray = Array.init ncores (fun () -> Condition.create ()) 
	let work_queue_aray = Array.init ncores (fun () -> Queue.create ()) 
	let true_merge_aray = Array.init ncores (fun () -> Hashtbl.create 10)
	let false_merge_aray = Array.init ncores (fun () -> Hashtbl.create 10)
	let eu_ar_merge_aray = Array.init ncores (fun () -> Hashtbl.create 10)
	let result_mutex = Mutex.create ()
	let result_signal = Condition.create ()
	let in_global_merge e levl = 
		try
			let index = (Hashtbl.hash e) mod ncores in
			State_set.mem e (Hashtbl.find (eu_ar_merge_aray.(index)))
		with
		| Not_found -> false
	let add_to_global_merge e levl = 
		let index = (Hashtbl.hash e) mod ncores in
		try
			let orig_set = Hashtbl.find (eu_ar_merge_aray.(index)) levl in
			Hashtbl.replace (eu_ar_merge_aray.(index)) levl (State_set.add e orig_set)
		with
		| Not_found -> 
			Hashtbl.replace (eu_ar_merge_aray.(index)) levl (State_set.singleton e)
	let clear_global_merge levl = 
		for i = 0 to ncores - 1 do
			Hashtbl.replace (eu_ar_merge_aray.(index)) levl (State_set.empty)
		done
	let is_in_true_merge e levl = 
		try
			let index = (Hashtbl.hash e) mod ncores in
			State_set.mem e (Hashtbl.find (true_merge_aray.(index)))
		with
		| Not_found -> false
	let is_in_false_merge e levl = 
		try
			let index = (Hashtbl.hash e) mod ncores in
			State_set.mem e (Hashtbl.find (false_merge_aray.(index)))
		with
		| Not_found -> false
	let add_to_true_merge e levl = 
		let index = (Hashtbl.hash e) mod ncores in
		try
			let orig_set = Hashtbl.find (true_merge_aray.(index)) levl in
			Hashtbl.replace (true_merge_aray.(index)) levl (State_set.add e orig_set)
		with
		| Not_found -> 
			Hashtbl.replace (true_merge_aray.(index)) levl (State_set.singleton e)
	let add_to_false_merge e levl = 
		let index = (Hashtbl.hash e) mod ncores in
		try
			let orig_set = Hashtbl.find (false_merge_aray.(index)) levl in
			Hashtbl.replace (false_merge_aray.(index)) levl (State_set.add e orig_set)
		with
		| Not_found -> 
			Hashtbl.replace (false_merge_aray.(index)) levl (State_set.singleton e)
	let get_result () = 
		Mutex.lock result_mutex;
		Condition.wait result_signal result_mutex;
		Mutex.unlock result_mutex;
		match !result with
		| None -> printf "could not got result, exiting...\n"; exit 1
		| Some r -> r
	let stop_world r = 
		result := Some r;
		for i = 0 to ncores - 1 do 
			Mutex.lock mutex_aray.(i);
			Queue.push Terminate work_queue_aray.(i);
			Condition.signal condition_aray.(i);
			Mutex.unlock mutex_aray.(i)
		done
	let work f i = 
		let running = ref true in
		while !running do
			if Queue.is_empty work_queue_aray.(i) then begin
				Mutex.lock mutex_aray.(i);
				Condition.wait condition_aray.(i) mutex_aray.(i);
				Mutex.unlock mutex_aray.(i)
			end else begin
				let elem = ref Terminate in
				Mutex.lock mutex_aray.(i);
				elem := Queue.pop work_queue_aray.(i);
				Mutex.unlock mutex_aray.(i);
				match !elem with
				| Terminate -> running := false
				| New_element e -> 
					let oes = f e in begin
						match oes with
						| None -> (*means a result has been produced*)
							Mutex.lock result_mutex;
							Condition.signal result_signal;
							Mutex.unlock result_mutex;
							Queue.clear work_queue_aray.(i)
						| Some es -> 
							State_set.iter 
								(fun a -> 
									let index = (Hashtbl.hash a) mod ncores in
									if index = i then
										Queue.push (New_element a) work_queue_aray.(index)
									else begin
										Mutex.lock mutex_aray.(index);
										Queue.push (New_element a) work_queue_aray.(index);
										Condition.signal condition_aray.(index);
										Mutex.unlock mutex_aray.(index)
									end
								) 
								es
					end
			end
		done
		
end

