Differences between Paper and Implementation
============================================

.. note::
   The information contained in the section is valid for the version Gillian that is tagged ``pldi-20``. The implementation may change in the future and implementation might get further away or closer to what the paper says in the future.

In the paper, the formalisation of GIL and the associated meta-theory is streamlined for clarity. The implementation follows the same principles, but is, expectedly, more complex. Here, we outline the main differences.

The GIL Syntax
--------------

Here is how the paper defines the GIL syntax:

    .. math::
       \begin{array}{lcl}
       v \in \mathcal{V} & \triangleq & n \in \mathcal{N} \mid s \in \mathcal{S} \mid b \in \mathcal{B} \mid l, \varsigma \in \mathcal{U} \mid \tau \in \mathcal{T} \mid f \in \mathcal{F} \mid \bar v \\
       e \in \mathcal{E} & \triangleq & v \mid x \in \mathcal{X} \mid \ominus e \mid e_1 \oplus e_2\\
       c \in \mathcal{C}_A & \triangleq & x := e \mid \mathsf{ifgoto}\ e\ i \mid x := e(e') \mid \mathsf{return}\ e \mid \mathsf{fail}\ e \\
                         &            & \mid \mathsf{vanish} \mid x := \alpha(e) \mid x := \mathsf{uSym}_j \mid x := \mathsf{iSym}_j \\
      proc \in \mathcal{P}roc_A & \triangleq & \mathsf{proc}\ f(x)\{\bar c\}\\
      \mathsf p \in \mathcal{P}rog_A & : & \mathcal{F} \rightharpoonup \mathcal{P}roc_A
      \end{array}

    GIL values, :math:`v \in \mathcal{V}`, include numbers, strings, booleans, uninterpreted symbols, types, procedure identifiers, and lists of values. Types are standard: they include, for example, the types of numbers, strings, booleans, and lists.

    GIL expressions, :math:`e \in \mathcal{E}`, include values, program variables :math:`x`, and various unary and binary operators.

    GIL commands include, first of all, the standard variable assignment, conditional goto, and dynamic procedure call. We assume single-parameter procedures in the meta-theory; in the implementation, we allow for multiple parameters. Next, the :math:`\mathsf{return}` command terminates the execution of the current procedure; :math:`\mathsf{fail}` terminates the execution of the entire program with an error; and :math:`\mathsf{vanish}` silently terminates program execution without generating a result. Finally, we have three GIL-specific commands: action execution, :math:`x := \alpha(e)`, which executes the action :math:`\alpha \in A` with the argument obtained by evaluating :math:`e`; and two analysis-related commands, :math:`x := \mathsf{uSym}_j` and :math:`x := \mathsf{iSym}_j` , which use Gillian’s built-in symbol generator to generate fresh symbols, similarly to the :math:`\mathsf{gensym}` command of Lisp and Racket. We call the symbols created using :math:`\mathsf{uSim}_j` uninterpreted, and the symbols created using :math:`\mathsf{iSym}_j` interpreted symbols. The difference between them is in the way in which they are used in the symbolic analysis: intuitively, we use uninterpreted symbols to represent instantiation-specific constants or unique memory constituents; and interpreted symbols to represent logical variables, as in standard symbolic execution literature.

    A GIL procedure, :math:`proc \in \mathcal{P}roc_A` is of the form :math:`\mathsf{proc}\ f(x)\{c\}`, where :math:`f` is its identifier, :math:`x` is its formal parameter, and its body :math:`c` is a sequence of GIL commands. A GIL program, :math:`p \in \mathcal{P}rog_A`, is a finite partial function, mapping procedure identifiers to their corresponding procedures.

The actual implementation of GIL slightly differs from this.

Commands
^^^^^^^^

Let us start by focusing on commands, here is how commands are defined in the implementation:

.. code-block:: ocaml

   (* Cmd.ml *)
   type 'label t =
     | Skip                                                                      (** Skip                              *)
     | Assignment    of string * Expr.t                                          (** Assignment                        *)
     | LAction       of string * string * Expr.t list                            (** Local Actions                     *)
     | Logic         of LCmd.t                                                   (** GIL Logic commands                *)
     | Goto          of 'label                                                   (** Unconditional goto                *)
     | GuardedGoto   of Expr.t * 'label * 'label                                 (** Conditional goto                  *)
     | Call          of
         string * Expr.t * Expr.t list * 'label option * logic_bindings_t option (** Procedure call                    *)
     | ECall         of string * Expr.t * Expr.t list * 'label option            (** External Procedure call           *)
     | Apply         of string * Expr.t * 'label option                          (** Application-style procedure call  *)
     | Arguments     of string                                                   (** Arguments of the current function *)
     | PhiAssignment of (string * Expr.t list) list                              (** PHI assignment                    *)
     | ReturnNormal                                                              (** Normal return                     *)
     | ReturnError                                                               (** Error return                      *)
     | Fail          of string * Expr.t list                                     (** Failure                           *)

The assignment is as described in the paper. There is an additional ``Skip`` command, that does nothing.

In action call, ``LAction(x, a, el)``, ``x`` is the variable name the result is going to be assigned to, ``a`` is the name of the action and ``el`` is a *list* of parameters. Like function calls, the paper presents a simplified version where actions can only take one parameter. Moreover, there is not ``action`` type, but actions are denoted by their name which is a string.

Instead of an ``ifgoto e i`` command there are two commands :

- ``goto lab`` that jumps to label ``lab``
- ``goto [e] lab1 lab2`` that jumps either to ``lab1`` or to ``lab2`` depending on the boolean ``e`` evaluates to
  Note that using ``ifgoto`` or these two kinds of gotos is equivalent. Moreover, in the implementation, the type of labels is polymorphic. ``string Cmd.t`` corresponds to "labeled commands", meaning one can annotate commands with string labels. ``int Cmd.t`` corresponds to "indexed commands", meaning ``goto j`` jumps the the ``j``-th command of the current procedure. We write GIL programs with labeled commands for readability, and translate to indexed commands for efficiency.

There is an additional ``Argument`` command that returns the list of arguments given to the current procedure, and a ``PhiAssignment`` that can be used for Static Single Assignment style programming.

The implementation of GIL comes with a better treatment of errors. There are two return commands:

- ``ReturnNormal``
- ``ReturnError``
  In both cases, the value returned is the one contained in the special ``"ret"`` variable. Both command return as explained in the paper, in an intuitive way, but they set the return mode either in ``Normal`` mode or in ``Error`` mode. If the function returns in ``Normal`` mode, the program execution continues normally, otherwise, it depends on how the function was called.

Let us take a closer look at the ``Call`` command, and describe the 5 arguments of ``Call(x, f, el, lab_opt, bindings)``.

- ``x`` is the name of the variable in which the result will be stored
- ``f`` is the expression that should resolve to the procedure identifier (a string)
- ``el`` is the list of expressions passed as arguments to the procedure
- ``lab_opt`` is an optional label to which the execution will jump if the called procedure returns in ``Error`` mode.
- Some logic bindings that are useful for verification, but out of scope for the PLDI-2020 paper

``Fail`` is very similar to the ``fail`` command described in the paper: it terminates the execution of the entire program in failure mode. However, it takes an additional parameter to the "failing value", which is a string that contains an error name or error message.

The implementation also has an external call mechanism (``ECall``) that is used to model ``eval`` in JavaScript, and could be used to model some system calls in Gillian-C.

``Apply`` is an application-style procedure calls. It takes only one expression as parameter, which should evaluate to the list of argument that will be passed to the procedure.

There are ``Logic`` commands, such as ``Assume`` or ``Assert``. But there are also more kinds of logic commands that are used for the verification mode of Gillian.

Finally the paper describes three more kinds of commands that are not in the implementation. The first one is ``vanish`` that is trivially replaced by ``Assume False`` in the implementation. Then, ther are the ``uSym`` and ``iSym`` commands which are mainly theoretical devices that ensure soundness in the presence of fresh-value generation. In the implementation, we provide an allocation mechanism that allows the creators of Gillian instantiations to generate fresh interpreted and uninterpreted symbols.

Procedures and programs
^^^^^^^^^^^^^^^^^^^^^^^

As explained earlier, there is no defined set ``A`` of actions, actions are denotted by their name, a string. Also, the procedures and programs contain much more information than what is in the paper.

.. code-block:: ocaml

   type ('annot, 'label) proc = {
     proc_name : string;
     proc_body : ('annot * 'label option * 'label Cmd.t) array;
     proc_params : string list;
     proc_spec : Spec.t option;
   }
   
   type ('annot, 'label) prog = {
     imports : string list;
     lemmas : (string, Lemma.t) Hashtbl.t;
     preds : (string, Pred.t) Hashtbl.t;
     only_specs : (string, Spec.t) Hashtbl.t;
     procs : (string, ('annot, 'label) Proc.t) Hashtbl.t;
     macros : (string, Macro.t) Hashtbl.t;
     bi_specs : (string, BiSpec.t) Hashtbl.t;
     proc_names : string list;
     predecessors : (string * int * int, int) Hashtbl.t;
   }

Procedures have a name, a body and parameters as described in the paper. However, each command in the body is also annotated with an opaque value that can be decided by the user (it has the ``'annot`` polymorphic type). These annotations can be used to keep information during execution that helps understanding the result of an analysis. Every command is also attached to a label, that has polymorphic type ``'label``. Most often, we use ``string`` labels for labeled programs and ``int`` labels for labeled programs as explained above. Finally, procedures can also have specifications that are used for verification but are out of scope for the PLDI2020 paper.

Programs are not just a map from procedure identifiers to procedures. There are also:

- ``lemmas``, ``predicates`` and ``specifications`` that are used for verification (out of scope her)
- ``bi_specs`` which are precomputed hints for automatic compositional testing
- ``macros`` which are used to define syntactic sugar over lists of logic commands, useful for readability, and unfolded at execution time
- A ``predecessors`` table used for the Phi Assignment

The Memory Interfaces
---------------------

Here is how Memory models are defined in the paper:

    **Definition** *(Concrete Memory Model)*: A concrete memory model, :math:`M \in \mathbb{M}`, is a triple :math:`\langle |M|, A, \underline{\mathsf{ea}}\rangle`, consisting of a set of concrete memories, :math:`|M| \ni \mu`, a set of actions :math:`A \ni \alpha`, and the action execution function :math:`\underline{\mathsf{ea}} : A \rightarrow |M| \rightarrow \mathcal{V} \rightarrow \wp(|M| \times \mathcal{V})`, pretty-printed :math:`\mu.\alpha(v) \rightsquigarrow (\mu', v)`.

    **Definition** *(Symbolic Memory Model)*: A symbolic memory model, :math:`\hat M \in \mathbb{M}`, is a triple :math:`\langle |\hat M|, A, \hat{\underline{\mathsf{ea}}}\rangle`, consisting of a set of symbolic memories, :math:`|\hat M| \ni \hat \mu`, a set of actions :math:`A \ni \alpha`, and the action execution function :math:`\underline{\mathsf{ea}} : A \rightarrow |M| \rightarrow \mathcal{V} \rightarrow \wp(|M| \times \mathcal{V})`, pretty-printed :math:`\mu.\alpha(v) \rightsquigarrow (\mu', v)`, pretty-printed :math:`\hat \mu.\alpha(\hat e) \rightarrow (\mu', \hat e', \pi ')`.

In the implementation, Concrete Memory Models and Symbolic Memory Models have an interface a bit more complex. The complete interface can be found in the files ``GillianCore/engine/SymbolicSemantics/SMemory.ml`` and ``GillianCore/engine/ConcreteSemantics/CMemory.ml``.

These interfaces do export:

- ``type t``, the type of memories, which correspond respectively to :math:`|M|` and :math:`|\hat M|`
- ``val execute_action: string -> t -> vt list -> action_ret`` for the concrete memory models, which corresponds to the theoretical definition apart from the fact that actions are represented by their ``string`` name and that concrete actions can return an error, which is used for automatic compositional testing (out of scope here)
- ``val execute_action: string -> t -> PFS.t -> TypeEnv.t -> vt list -> action_ret`` for the symbolic memory models, which correspond to the theoretical definition apart from actions that are represented by their ``string`` names, the fact that the actions can return errors which are used for automatic compositional testing (out of scope here), and the path conditions (:math:`\pi`) are split into two parts : ``PFS.t`` which are set of pure formulae and ``TypeEnv.t`` which are special kind of pure formulae corresponding to the type of values.

These interfaces export more definitions.
Since, for efficiency reasons, the type of memories can be mutable, the user must define an ``init`` function and a ``copy`` function. The user also has to define pretty printers for its state, which are used for the log files.

Finally, there are a lot of definitions (``ga_to_...``, ``is_overlaping_asrt``, ``assertions``, ``mem_constraints``, ``type err_t``, etc.) that are used either for verification or automatic compositional testing and are not presented in the PLDI20 paper because they are out of scope.

The State Model interface
-------------------------

In the paper, the state model interface is defined as below:

    **Definition** *(State Model)*: A state model :math:`S \in \mathbb{S}` is a quadruple :math:`\langle|S|, \mathsf{V}, A, \mathsf{ea}\rangle`, consisting of: **(1)** a set of states on which GIL programs operate, :math:`|S| \ni \sigma`; **(2)** a set of values stored in those states, :math:`\mathsf{V} \ni v`; **(3)** a set of actions that can be performed on those states, :math:`A \ni \alpha`; and **(4)** a function :math:`\mathsf{ea}: a \rightarrow |S| \rightarrow \mathsf{V} \rightarrow \wp(|S| \times \mathsf{V})` for execution actions on states. All GIL states must contain an internal representation of a *variable store*, denoted by :math:`\rho`, assigning values to program variables.

    We write :math:`\sigma.\alpha(v) \rightsquigarrow (\sigma', v')` to mean :math:`(\sigma', v') \in \mathsf{ea}(\alpha, \sigma, v)`, and refer to :math:`\sigma'` as the state output and to :math:`v'` as the value output of :math:`\alpha`.

It is also added that:

    A state model :math:`S = \langle |S|, \mathsf V, A, \mathsf{ea}\rangle` is *proper* if and only if its set of actions, A, includes the following distinguished actions/families of actions:

    - :math:`\{ \mathsf{setVar}_x \}_{x \in \mathcal{X}}` for updating the value of :math:`x` in the store of a given state, denoted by :math:`\sigma.\mathsf{setVar}_x(v)`;
    - :math:`\mathsf{setStore}`, for replacing the entire store of a given state with a new store, denoted by :math:`\sigma.\mathsf{setStore}(\rho)`;
    - :math:`\mathsf{getStore}`, for obtaining the store of the given state, denoted by :math:`\sigma.\mathsf{getStore}()`;
    - :math:`\{ \mathsf{eval}_e \}_{e \in \mathcal{E}}` for evaluationg the expression :math:`e` in a given state, denoted by :math:`\sigma.\mathsf{eval}_e(-)`;
    - :math:`\mathsf{assume}`, for extending the given state with the information denoted by its argument value, denoted by :math:`\sigma.\mathsf{assume}(v)`;
    - :math:`\mathsf{uSym}` and :math:`\mathsf{iSym}`, for generating new uninterpreted and interpreted symbols, respectively. From now on, we work with proper state models.

In the implementation, the interface of state models, available in ``GillianCore/engine/GeneralSemantics/State.ml`` is a bit difference and more complex.

First of all, the state interface defines "proper state models" in the first place. However, these state models do not define "families of actions". For example, ``eval_expr`` is one particular function exposed by the state interface, and has the following signature:

.. code-block:: ocaml

   val eval_expr : t -> Expr.t -> vt

``setVar`` is defined in terms of ``setStore`` and ``getStore`` directly by the interpreter:

.. code-block:: ocaml

   let update_store (state : State.t) (x : string) (v : Val.t) : State.t =
       let store = State.get_store state in
       let _ = Store.put store x v in
       let state' = State.set_store state store in
       state'

Note that variables are designated by their string names. Also note the usage of ``Store.put``: stores have their own interface in the implementation which greatly simplify their usage. Setting a variable in the store is simply getting the store of the state, setting the variable to the correct value in the store and putting that new obtained store back in the state.

States can be mutable to improve the performances, and therefore there is an ``init`` and a ``copy`` function.

The ``execute_action`` function defined in the state interface corresponds only to the lifting of user-defined memory-model actions, given that all necessary actions to have a proper state are defined as functions of their own.

.. code-block:: ocaml

   val execute_action : string -> t -> vt list -> action_ret

Once again, actions are designated by their string names, and actions can return either a list of successful state or some errors that can be used for automatic compositional testing.

Finally, there are a lot of different functions that do not correspond to any aspect of the state models presented in the paper such as ``unify_assertion``, ``produce_posts``, ``apply_fixes``, etc. which are useful either for the verification mode or the automatic compositional testing mode of Gillian, and are out of scope for the Gillian PLDI2020 paper.

Allocators
----------

In the paper allocators have the following definition:

    An allocator :math:`AL \in \mathbb{A}\mathbb{L}` is a triple :math:`\langle|AL|, \mathsf Y, \mathsf{alloc}\rangle`, consisting of: **(1)** a set :math:`|AL|\ni \xi` of allocation records; **(2)** a set :math:`Y` of all values that are allowed to be allocated; and **(3)** an allocation function:

    .. math::
       \mathsf{alloc}: |AL| \rightarrow \mathbb{N} \rightarrow \wp(\mathsf Y) \rightharpoonup |AL|\times V


    pretty-printed as :math:`\xi.\mathsf{alloc}(j)\rightharpoonup_{\mathsf Y}(\xi', y)`, which takes an allocation record :math:`\xi`, a, allocation site :math:`j`, and an allocation range :math:`Y \subseteq \mathsf Y`, and returns a fresh value :math:`y \in Y`, together with the appropriately updated allocation record :math:`\xi'`.

    Intuitively, an allocation record maintains information about already allocated values. This apporach is complementary to `the free set approach <https://doi.org/10.1007/978-3-540-78499-9_15>`_, where information is maintained about values that can still be allocated. An allocation site :math:`j` is the program point associated with either the :math:`\mathsf{uSym}_j` or the :math:`\mathsf{iSym}_j` command.

This could be interpreted in terms of OCaml module signature as:

.. code-block:: ocaml

   module type Allocator = sig
     type t    (** Type of allocation records     *)
     type us_t (** Type of uninterpreted symbols **)
     type is_t (**  Type of interpreted symbols   *)
   
     val alloc_us : t -> int -> t * us_t
     val alloc_is : t -> int -> t * is_t
   end

However, for efficiency, we chose this implementation:

.. code-block:: ocaml

   (* Allocator.ml *)
   module type S = sig
     type t                   (** Type of value to allocate *)
   
     val alloc : unit -> t    (** Allocation function *)
     val dealloc : t -> unit  (** Deallocation function *)
     val eq : t -> t -> bool  (** Equality of values to allocate *)
     val reset : unit -> unit (** Reset this allocator *)
   end

The ``reset`` function is useful for bulk-testing. When running a new test, every allocator is reset.

The Abstract location allocator (in ``ALoc.ml``), which corresponds to uninterpreted symbols, are then initiated like this:

.. code-block:: ocaml

   include Allocators.Make_with_prefix
             (Basic ())
             (struct
               let prefix = Names.aloc_
             end)

Where ``Make_with_prefix`` is a functor that takes:

- An abstract Allocator ``AL`` that produces values which can be stringified.
- A string prefix

and it returns an Allocator that allocates strings of the form ``PREFIX_A`` where ``PREFIX`` is the given prefix and ``A`` is a stringification of the allocated by ``AL``.

In this case, as the ``AL`` parameter, we use ``Basic ()`` which instantiates an abstract allocator module that internally just allocates integers.