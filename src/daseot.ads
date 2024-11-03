private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;

generic
   type Scalar (<>) is private;
   with function Image (Value : Scalar) return String is <>;
   --  One line UTF-8 encoded
package Daseot with Preelaborate is

   --  This package uses thread-unsafe by-copy semantics

   type Kinds is (Atom_Kind, Dict_Kind, List_Kind);

   subtype Composite_Kinds is Kinds range Dict_Kind .. List_Kind;

   -------------
   --  Trees  --
   -------------

   type Tree is tagged private;

   function Empty_Tree return Tree;

   function Is_Empty (This : Tree) return Boolean;

   function Is_Populated (This : Tree) return Boolean is (not This.Is_Empty);

   function Image (This : Tree) return String;
   --  Multi-line UTF-8-encoded

   function Kind (This : aliased Tree) return Kinds
     with Pre => This.Is_Populated;

   type Tree_Array is array (Positive range <>) of Tree;

   procedure Set (This   : Tree;
                  Value  : Scalar;
                  Retype : Boolean := False);

   function Set (This   : Tree;
                 Value  : Scalar;
                 Retype : Boolean := False) return Tree;

   function Get (This : aliased Tree) return Scalar
     with Pre => This.Kind = Atom_Kind;

   ------------
   --  Node  --
   ------------

   --  Edition of trees is done through a Node, which should never be
   --  stored, as it is a transitory reference into the tree (like a cursor).

   type Node (<>) is tagged limited private with
     Iterable =>
       (First       => First,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Element);

   function Root (This : aliased Tree'Class) return Node;

   function Is_Empty (This : Node) return Boolean;
   --  True for an unset root or for empty Dict or List

   function Is_Populated (This : Node) return Boolean
   is (not This.Is_Empty);

   function Is_Root (This : Node) return Boolean;

   function Kind (This : Node) return Kinds;
   --  Will raise for an empty root

   function Copy (This : Node'Class) return Tree;
   --  Deep copy

   -------------
   --  Atoms  --
   -------------

   procedure Set (This   : Node;
                  Value  : Scalar;
                  Retype : Boolean := False);

   function Set (Value : Scalar) return Tree;

   function Get (This : Node) return Scalar;

   -------------
   --  Dicts  --
   -------------

   subtype Keys is String;

   function Empty_Dict return Tree;
   function Dict return Tree renames Empty_Dict;

   procedure Map (This   : aliased Node;
                  Key    : Keys;
                  Val    : Node'Class;
                  Retype : Boolean := False);

   procedure Map (This   : aliased Node;
                  Key    : Keys;
                  Val    : Scalar;
                  Retype : Boolean := False);

   function Map (This   : aliased Node;
                 Key    : Keys;
                 Val    : Node'Class;
                 Retype : Boolean := False)
                 return Node;
   --  Maps to a copy of Val

   function Map (This   : aliased Node;
                 Key    : Keys;
                 Val    : Scalar;
                 Retype : Boolean := False)
                 return Node;
   --  Return the updated Node, for chaining

   -------------
   --  Lists  --
   -------------

   subtype Indices is Positive;

   function Empty_List return Tree;
   function List return Tree renames Empty_List;

   procedure Append (This   : aliased Node;
                     Val    : Node;
                     Retype : Boolean := False);

   procedure Append (This   : aliased Node;
                     Val    : Scalar;
                     Retype : Boolean := False);

   function Append (This   : aliased Node;
                    Val    : Node;
                    Retype : Boolean := False)
                    return Node;

   function Append (This   : aliased Node;
                    Val    : Scalar;
                    Retype : Boolean := False)
                    return Node;
   --  For chaining

   function To_List (This : Tree_Array) return Tree with
     Pre  => (for all E of This => E.Is_Populated),
     Post => To_List'Result.Root.Kind = List_Kind;

   -----------------
   --  Internals  --
   -----------------

   --  These are exposed due to Ada technicalities but are not needed by
   --  clients.

   type Mutable_Node (<>) is private;

   type Cursor (<>) is private;

   function First (This : Node) return Cursor;

   function Next (This : Node; C : Cursor) return Cursor;

   function Has_Element (This : Node; C : Cursor) return Boolean;

   function Element (This : Node; C : Cursor) return Node;

private

   Unimplemented : exception;

   package Base_Nodes is

      type Base_Node is abstract tagged null record;

      function Ref (This : aliased Base_Node'Class) return Node;

      function Is_Empty (This : Base_Node) return Boolean is abstract;

   end Base_Nodes;

   subtype Base_Node is Base_Nodes.Base_Node;

   use all type Base_Node;

   --  NOTE: Node is actually a reference type, out of the Base_Node hierarchy.
   --  However, to maintain the appropriate naming for library clients, it is
   --  called Node.

   type Node (Ptr : access Base_Node'Class) is tagged limited null record
     with Implicit_Dereference => Ptr;

   function Base (This : Node'Class) return Base_Node'Class
   is (This.Ptr.all);

   package Node_Holders is
     new Ada.Containers.Indefinite_Holders (Base_Node'Class);

   package Root_Nodes is

      type Root_Node is new Base_Node with record
         Root : Node_Holders.Holder;
      end record;

      overriding function Is_Empty (This : Root_Node) return Boolean
      is (This.Root.Is_Empty);

      function Real_Ref (This : aliased Root_Node'Class) return Node;

      procedure Store (This : in out Root_Node'Class;
                       Ref  : Node);

   end Root_Nodes;

   subtype Root_Node is Root_Nodes.Root_Node;

   function As_Root (This : Node'Class) return access Root_Node
   is (Root_Node (This.Ptr.all)'Unchecked_Access);

   --  We use a doubly nested root so we can always return a reference to the
   --  root node.

   type Tree is tagged record
      R : Root_Node;
   end record;

   package Node_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Keys, Base_Node'Class);

   package Node_Vectors is
      new Ada.Containers.Indefinite_Vectors (Indices, Base_Node'Class);

   package Scalar_Holders is new Ada.Containers.Indefinite_Holders (Scalar);

   function Assert_Mutable_Contents (This : Mutable_Node) return Boolean;

   type Mutable_Node (Kind : Kinds := Atom_Kind) is record
      case Kind is
         when Atom_Kind =>
            Value : Scalar_Holders.Holder;
         when Dict_Kind =>
            Dict  : Node_Maps.Map;
         when List_Kind =>
            List  : Node_Vectors.Vector;
      end case;
   end record
     with Type_Invariant => Assert_Mutable_Contents (Mutable_Node);

   type Real_Node is new Base_Node with record
      Data : Mutable_Node;
   end record;

   overriding function Is_Empty (This : Real_Node) return Boolean;

   function New_Atom (Value : Scalar) return Real_Node;

   function New_Dict return Real_Node;

   function New_List return Real_Node;

   function As_Real (This : Node'Class) return access Real_Node
   is (Real_Node (This.Ptr.all)'Unchecked_Access);

   type Impls is (Root, Real);

   function Impl (This : Node'Class) return Impls
   is (if This.Ptr.all in Real_Node'Class then
          Real
       elsif This.Ptr.all in Root_Node'Class then
          Root
       else raise Program_Error);

   type Cursor (Kind : Kinds) is record
      case Kind is
         when Atom_Kind =>
            Visited     : Boolean := False;
         when Dict_Kind =>
            Dict_Cursor : Node_Maps.Cursor;
         when List_Kind =>
            List_Cursor : Node_Vectors.Cursor;
      end case;
   end record;

   -------------
   --  IMPLS  --
   -------------

   -----------------------------
   -- Assert_Mutable_Contents --
   -----------------------------

   function Assert_Mutable_Contents (This : Mutable_Node) return Boolean
   is (case This.Kind is
          when Atom_Kind =>
             not This.Value.Is_Empty,
          when Dict_Kind =>
             (for all E of This.Dict => E in Real_Node'Class),
          when List_Kind =>
             (for all E of This.List => E in Real_Node'Class));

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree
   is (R => <>);

   -----------
   -- First --
   -----------

   function First (This : Node) return Cursor
   is (if This.Ptr.all in Real_Node then
         (case Real_Node (This.Ptr.all).Data.Kind is
             when Atom_Kind => (Kind => Atom_Kind, others => <>),
             when Dict_Kind => raise Unimplemented,
             when List_Kind =>
               (Kind        => List_Kind,
                List_Cursor => Real_Node (This.Ptr.all).Data.List.First))
       else
          raise Unimplemented
      );

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Node) return Boolean
   is (This.Ptr.Is_Empty);

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (This : Real_Node) return Boolean
   is (This.Data.Dict.Is_Empty);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Tree) return Boolean
   is (This.R.Is_Empty);

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Node) return Boolean
   is (This.Ptr.all in Root_Node);

   ----------
   -- Kind --
   ----------

   function Kind (This : aliased Tree) return Kinds
   is (This.Root.Kind);

   --------------
   -- New_Atom --
   --------------

   function New_Atom (Value : Scalar) return Real_Node
   is (Data => (Kind  => Atom_Kind,
                Value => Scalar_Holders.To_Holder (Value)));

   --------------
   -- New_Dict --
   --------------

   function New_Dict return Real_Node
   is (Data => (Kind   => Dict_Kind,
                others => <>));

   --------------
   -- New_List --
   --------------

   function New_List return Real_Node
   is (Data => (Kind   => List_Kind,
                others => <>));

   ----------
   -- Next --
   ----------

   function Next (This : Node; C : Cursor) return Cursor
   is (case C.Kind is
          when Atom_Kind => (Kind => Atom_Kind, Visited => True),
          when List_Kind =>
            (Kind        => List_Kind,
             List_Cursor => Node_Vectors.Next (C.List_Cursor)),
          when others => raise Unimplemented
      );

   ----------
   -- Root --
   ----------

   function Root (This : aliased Tree'Class) return Node
   is (Ptr => This.R'Unrestricted_Access);

end Daseot;
