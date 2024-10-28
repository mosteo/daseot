private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Ordered_Maps;

generic
   type Scalar (<>) is private;
   with function Image (Value : Scalar) return String is <>;
   --  One line UTF-8 encoded
package Daseot is

   --  This package uses thread-unsafe by-copy semantics

   -------------
   --  Trees  --
   -------------

   type Tree is tagged private;

   function Is_Empty (This : Tree) return Boolean;

   function Is_Populated (This : Tree) return Boolean is (not This.Is_Empty);

   function Image (This : Tree) return String;
   --  Multi-line UTF-8-encoded

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

   function Root (This : aliased in out Tree'Class) return Node;

   type Kinds is (Atom_Kind, Dict_Kind, List_Kind);

   subtype Composite_Kinds is Kinds range Dict_Kind .. List_Kind;

   function Kind (This : Node) return Kinds;

   -------------
   --  Atoms  --
   -------------

   procedure Set (This : Node; Value : Scalar);

   function Get (This : Node) return Scalar;

   -------------
   --  Dicts  --
   -------------

   subtype Keys is String;

   function Is_Empty (This : Node) return Boolean with
     Pre => This.Kind in Composite_Kinds;

   function Is_Populated (This : Node) return Boolean
   is (not This.Is_Empty);

   procedure Set (This : aliased Node;
                  Key  : Keys;
                  Val  : Scalar);

   function Set (This : aliased Node;
                 Key  : Keys;
                 Val  : Scalar)
                 return Node;
   --  Return the updated Node, for chaining

   -------------
   --  Lists  --
   -------------

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

   type Base_Node is abstract tagged null record;

   function Is_Empty (This : Base_Node) return Boolean is abstract;

   type Node (Ptr : access Base_Node'Class) is tagged limited null record
     with
       Implicit_Dereference => Ptr,
       Type_Invariant => Check_Node (Node.Ptr.all);

   function Ref (This : aliased Base_Node'Class) return Node'Class
   is (Node'(Ptr => This'Unrestricted_Access));

   package Node_Holders is
     new Ada.Containers.Indefinite_Holders (Base_Node'Class);

   subtype Placeholder is Node_Holders.Holder; -- TODO: remove

   type Empty is new Base_Node with null record;

   overriding function Is_Empty (This : Empty) return Boolean is (True);

   Empty_Node : constant Empty := (null record);

   type Root_Node is new Base_Node with record
      Root : Node_Holders.Holder := Node_Holders.To_Holder (Empty_Node);
   end record;

   --  We use a doubly nested root so we can always return a reference to the
   --  root node.

   type Tree is tagged record
      Root : aliased Root_Node;
   end record
     with Type_Invariant => not Root.Root.Is_Empty;

   package Node_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Keys, Base_Node'Class);

   package Scalar_Holders is new Ada.Containers.Indefinite_Holders (Scalar);

   function Assert_Mutable_Contents (This : Mutable_Node) return Boolean;

   type Mutable_Node (Kind : Kinds := Atom_Kind) is record
      case Kind is
         when Atom_Kind =>
            Value : Scalar_Holders.Holder;
         when Dict_Kind =>
            Dict  : Node_Maps.Map;
         when List_Kind =>
            List  : Placeholder;
      end case;
   end record
     with Type_Invariant => Assert_Mutable_Contents (Mutable_Node);

   type Real_Node is new Base_Node with record
      Data : Mutable_Node;
   end record;

   overriding function Is_Empty (This : Real_Node) return Boolean;

   function New_Atom (Value : Scalar) return Real_Node;

   function New_Dict return Real_Node;

   type Cursor (Kind : Kinds) is record
      case Kind is
         when Atom_Kind =>
            Visited     : Boolean := False;
         when Dict_Kind =>
            Dict_Cursor : Node_Maps.Cursor;
         when List_Kind =>
            List_Cursor : Placeholder;
      end case;
   end record;

   function Check_Node (This : Base_Node'Class) return Boolean
   is (This in Root_Node | Real_Node'Class);

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
          when others    => True);

   -------------
   -- Element --
   -------------

   function Element (This : Node; C : Cursor) return Node
   is (raise Unimplemented);

   -----------
   -- First --
   -----------

   function First (This : Node) return Cursor
   is (if This.Ptr.all in Real_Node then
         (case Real_Node (This.Ptr.all).Data.Kind is
             when Atom_Kind => (Kind => Atom_Kind, others => <>),
             when Dict_Kind => raise Unimplemented,
             when List_Kind => raise Unimplemented)
       else
          raise Unimplemented
      );

   ---------
   -- Get --
   ---------

   function Get (This : Node) return Scalar
   is (Real_Node (This.Ptr.all).Data.Value.Element);

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Node; C : Cursor) return Boolean
   is (raise Unimplemented);

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

   overriding function Is_Empty (This : Root_Node) return Boolean
   is (This.Root.Constant_Reference.Is_Empty);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Tree) return Boolean
   is (This.Root.Root.Constant_Reference in Empty);

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

   ----------
   -- Next --
   ----------

   function Next (This : Node; C : Cursor) return Cursor
   is (raise Unimplemented);

   ----------
   -- Root --
   ----------

   function Root (This : aliased in out Tree'Class) return Node
   is (Node'(Ptr => This.Root'Access));

end Daseot;
