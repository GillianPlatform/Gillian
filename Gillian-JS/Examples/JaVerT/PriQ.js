"use strict";

/**
	@pred Node(+n, pri:Num, val, next, np:Obj) :
		JSObjWithProto(n, np)     *
		DataProp(n, "pri",  pri)  * (0 <# pri) *
		DataProp(n, "val",  val)  *
		DataProp(n, "next", next) *
		((n, "insert") -> none);

	@pred NodePrototype(np:Obj) :
		JSObject(np) *
		DataProp(np, "insert", #insert_loc) *
		JSFunctionObject(#insert_loc, "np_insert", _, _, _) *
		((np, "pri") -> none) *
		((np, "val") -> none) *
		((np, "next") -> none);

	@pred NodeList(+nl, +np:Obj, max_pri:Num, length:Num) :
		(nl == null) * (max_pri == 0) * (length == 0),

		Node(nl, max_pri, #val, #next, np) * (0 <# max_pri) *
		NodeList(#next, np, #pri, #len_nl) * (#pri <=# max_pri) *
	    (0 <# length) * (length == #len_nl + 1);

	@pred Queue(+pq, qp, +np, max_pri : Num, length : Num) :
		JSObjWithProto(pq, qp) *
		DataProp(pq, "_head",  #head) *
		NodeList(#head, np, max_pri, length) *
		((pq, "enqueue") -> none) *
		((pq, "dequeue") -> none);

	@pred QueuePrototype(+qp, np, qfs_sc):
		JSObject(qp) *
		DataProp(qp, "enqueue", #enqueue_loc) * JSFunctionObject(#enqueue_loc, "enqueue", qfs_sc, _, _) *
		DataProp(qp, "dequeue", #dequeue_loc) * JSFunctionObject(#dequeue_loc, "dequeue", qfs_sc, _, _) *
		((qp, "_head") -> none) *
		sc_scope(enqueue, Node : #n, qfs_sc) * JSFunctionObject(#n, "Node", #node_sc, _, np) * NodePrototype(np);

	@pred PriorityQueueModule(pq, np) :
	  JSFunctionObject(pq, "PriorityQueue", #pq_sc, _, #pqp) *
	  QueuePrototype(#pqp, np, #qfs_sc) *
	  o_chains(PriorityQueue: #pq_sc, enqueue: #qfs_sc);
*/

/**
	@toprequires emp
	@topensures (
		scope(q : #q) * scope(r : #r) *
		scope(PriorityQueue : #pq_lib) * PriorityQueueModule (#pq_lib, #np) *
		Queue(#q, #pqp, #np, #pri_q, 1) *
		(ret == #r) * JSObject(#r) * DataProp(#r, "pri", _) * DataProp(#r, "val", _)
	)
*/

/**
	@id PQLib

	@pre  ObjectPrototype($lobj_proto)
  @post ObjectPrototype($lobj_proto) * PriorityQueueModule (ret, #np)
*/
var PriorityQueue = (function () {

	/**
		@id  Node

		@pre (
			(pri == #pri) * types(#pri : Num) * (0 <# #pri) * (val == #val) *
			((this, "pri") -> none) * ((this, "val") -> none) * ((this, "next") -> none) *
			((this, "insert") -> none) *
			JSObjWithProto(this, #np) * NodePrototype(#np) *
			ObjectPrototype($lobj_proto)
		)
		@post (
			Node(this, #pri, #val, null, #np) *
			NodePrototype(#np) *
			ObjectPrototype($lobj_proto) *
			(ret == undefined)
		)
	*/
	var Node = function (pri, val) {
		this.pri = pri;
		this.val = val;
		this.next = null;
	}

	/**
		@id np_insert

		@pre (
			(nl == #nl) *
			NodeList(#nl, #np, #pri_nl, #length) *
			Node(this, #npri, #nval, null, #np) *
			NodePrototype(#np) *
			(#pri_nl <# #npri)
		)
		@post (
			NodeList(this, #np, #npri, #length + 1) *
			NodePrototype(#np) *
			(ret == this)
		)

	  @pre (
			(nl == #nl) *
			NodeList(#nl, #np, #pri_nl, #length) *
			Node(this, #npri, #nval, null, #np) *
			NodePrototype(#np) *
			(#npri <=# #pri_nl)
	  )
	  @post (
			NodeList(#nl, #np, #pri_nl, #length + 1) *
			NodePrototype(#np) *
			(ret == #nl)
        )

      @pre (
			(nl == #nl) *
			NodeList(#nl, #np, #pri_nl, #length) *
			Node(this, #npri, #nval, null, #np) *
			NodePrototype(#np)
	  )
    @post
      (
        NodeList(this, #np, #npri, #length + 1) *
        NodePrototype(#np) *
        (ret == this) * (#pri_nl <# #npri)
      );
      (
			  NodeList(#nl, #np, #pri_nl, #length + 1) *
			  NodePrototype(#np) *
        (ret == #nl) * (#npri <=# #pri_nl)
      )
	*/
	Node.prototype.insert = function (nl) {
		if (nl === null) {
		   return this
		}

		if (this.pri > nl.pri) {
		   this.next = nl;
		   return this
		}

		var tmp = this.insert (nl.next);
		nl.next = tmp;
		return nl
	}

		/**
	    @id PriorityQueue

			@pre (
				ObjectPrototype($lobj_proto) *
        ((this, "_head") -> none) *
        ((this, "enqueue") -> none) *
        ((this, "dequeue") -> none) *
        JSObjWithProto(this, #pqp) *
        o_chains(enqueue: #qfs_sc, PQLib: $$scope) *
        QueuePrototype(#pqp, #np, #qfs_sc)
	    )
	    @post (
	    	ObjectPrototype($lobj_proto) *
	    	Queue(this, #pqp, #np, 0, 0) *
	    	QueuePrototype(#pqp, #np, #qfs_sc) *
	    	(ret == undefined)
	    )
	*/
	var PQ = function () {
		this._head = null;
	};

	/**
			@id enqueue

			@pre (
				(pri == #pri) * (0 <# #pri) * (val == #val) *
				Queue(this, #pqp, #np, #pri_q, #length) *
				QueuePrototype(#pqp, #np, #qfs_sc) *
				o_chains(enqueue: #qfs_sc, PQLib: $$scope) *
				(#pri <=# #pri_q) * ObjectPrototype($lobj_proto)
			)
			@post (
				(ret == undefined) *
				Queue(this, #pqp, #np, #pri_q, #length + 1) *
				QueuePrototype(#pqp, #np, #qfs_sc) * ObjectPrototype($lobj_proto)
			)

			@pre (
				(pri == #pri) * (0 <# #pri) * (val == #val) *
				Queue(this, #pqp, #np, #pri_q, #length) *
				QueuePrototype(#pqp, #np, #qfs_sc) *
				o_chains(enqueue: #qfs_sc, PQLib: $$scope) *
				(#pri_q <# #pri) * ObjectPrototype($lobj_proto)
			)
			@post (
				(ret == undefined) *
				Queue(this, #pqp, #np, #pri, #length + 1) *
				QueuePrototype(#pqp, #np, #qfs_sc) * ObjectPrototype($lobj_proto)
			)
	*/
	PQ.prototype.enqueue = function(pri, val) {
		var n = new Node(pri, val);
		this._head = n.insert(this._head);
	};


	/**
		@id dequeue

     @pre (
       Queue(this, #pqp, #np, #pri_q, #length) *
       QueuePrototype(#pqp, #np, #qfs_sc) *
       o_chains(enqueue: #qfs_sc, dequeue: $$scope) *
       (0 <# #length)
     )
     @post (
       Queue(this, #pqp, #np, #new_pri_q, #length - 1) *
       QueuePrototype(#pqp, #np, #qfs_sc) *
       (ret == #r) * JSObject(#r) *
       DataProp(#r, "pri", #pri_q) * DataProp(#r, "val", #some_val)
     )

	@pre (
       Queue(this, #pqp, #np, #pri_q, #length) * (#length == 0) *
       QueuePrototype(#pqp, #np, #qfs_sc) *
       o_chains(enqueue: #qfs_sc, dequeue: $$scope) *
       GlobalObject() * BI_ErrorObject()
     )
     @posterr (
       Queue(this, #pqp, #np, #pri_q, #length) *
       QueuePrototype(#pqp, #np, #qfs_sc) *
       (ret == #e) * ErrorObjectWithMessage(#e, "Queue is empty") *
       GlobalObject() * BI_ErrorObject()
     )
	*/
	PQ.prototype.dequeue = function () {
      /* @tactic assert DataProp(this, "_head", #nl) [bind: #nl];
                 unfold NodeList(#nl, #np, #pri_q, #length) */
	  if (this._head === null) {
	    throw new Error("Queue is empty");
	  }

	  var first = this._head;
	  this._head = this._head.next;
	  return {pri: first.pri, val: first.val};
	};

  return PQ;
})();