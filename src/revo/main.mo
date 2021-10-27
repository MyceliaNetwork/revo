import AssocList "mo:base/AssocList";
import Buffer "mo:base/Buffer";
import Char "mo:base/Char";
import Debug "mo:base/Debug";
import Float "mo:base/Float";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Int64 "mo:⛔";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Set "mo:base/TrieSet";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

actor {
    public type Identifier   = Nat;

    public type Value = {
        #Int        : Int64      ;
        #Scaled     : (Int64,Int64);
        #Function              ;
    };

    public type Action = {
        #Rotate    : Int64;
        #Move      : Int64;
        #PenUp          ;
        #PenDown        ;
        #Save           ;
        #Load           ;
        #Pass           ;
    };

    public type AlphabetValueVariable = {
        action    : Action;
        mapping   : [Identifier];
    };

    public type AlphabetValueConstant = Action;

    public type AlphabetValue = {
        #Variable   : AlphabetValueVariable;
        #Constant   : AlphabetValueConstant; 
    };

    public type AlphabetElement = {
        identifier : Identifier;
        value      : AlphabetValue;
    };

    public type System = {
        alphabet       : [AlphabetElement];
        owner          : Principal;
        axion          : [Identifier];
    };

    public type CreateSystem = {
        systemName     : Text;
        alphabet : [AlphabetElement];
        axion          : [Identifier];
    };

    public type Point = (Int, Int);
    public type Line = [Point];
    public type Shape = {
        #Circle : (Int, Int, Int);
    };

    public type DrawingElement = {
        #Shape : Shape;
        #Line : Line;
    };

    public type Drawing = [DrawingElement];

    public type Pen = {
            var penX       : Int64;
            var penY       : Int64;
            var directionX : Int64;
            var directionY : Int64;
            var mem        : ?(Int64, Int64, Int64, Int64);
            var isDown     : Bool;
            drawing    : Buffer.Buffer<Drawing>;
    };

    stable var systems = Trie.empty<Text, System>();

    public shared ({caller}) func createSystem(v : CreateSystem) : async Text {
        systems := Trie.put<Text, System>(systems, {key=v.systemName; hash=Text.hash(v.systemName)}, Text.equal, {
            alphabet = v.alphabet;
            owner = caller;
            axion = v.axion;
        }).0;
       "ok";
    };

    public type HttpRequest = {
        method      : Text;
        url         : Text;
        headers     : [(Text, Text)];
        body        : Blob; // or [Nat8]
    };

    public type HttpResponse = {
        body        : Blob;
        headers     : [(Text, Text)];
        status_code : Nat16;
    };

    private func getSystemMap(s : System) : HashMap.HashMap<Identifier, AlphabetElement> {
        let lookup = HashMap.HashMap<Identifier, AlphabetElement>(0, Nat.equal, Hash.hash);
        for (rule in s.alphabet.vals()) {
            lookup.put(rule.identifier, rule);
        };
        lookup;
    };

    private func walkSystem(v : System, iterations : Nat) : Text {
        var x      = 0;
        var out    = "";
        let lookup = getSystemMap(v);
        
        var dna = Buffer.Buffer<Identifier>(0);
        for (value in v.axion.vals()) {
            dna.add(value);
        };

        while (x < iterations) {
            let tmp = Buffer.Buffer<Identifier>(0);
            for (value in dna.vals()) {
                switch(lookup.get(value)) {
                    case null { Debug.print("Cannot find value " # debug_show value) };
                    case (?rule) {
                        switch (rule.value) {
                            case (#Constant(_)) {};
                            case (#Variable(v)) {
                                for (new in v.mapping.vals()) {
                                    tmp.add(new);
                                };
                            };
                        };
                    };
                };
            };
            dna := tmp;
            x += 1;
        };

        for (someValue in dna.vals()) {
            out := out # debug_show someValue;
        };

        return out;
    };

    private func degToRad(d : Int64) : Float {
        return  Float.fromInt64(d) * Float.pi / 180.0;
    };

    private func processAction(a : Action, p : Pen) : () {
        switch(a) {
            case (#Load) {
                switch (p.mem) {
                    case (?v) {
                        p.penX       := v.0;
                        p.penY       := v.1;
                        p.directionX := v.2;
                        p.directionY := v.3;
                        p.mem        := null;
                    };
                    case null {};
                }
            };
            case (#Save) {p.mem := ?(p.penX, p.penY, p.directionX, p.directionY)};
            case (#Move(scale)) {
                p.penX += scale * p.directionX;
                p.penY += scale * p.directionY;
            };
            case (#Rotate(amount)) {
                let r   = degToRad(amount);
                let x1  = (Float.fromInt64(p.penX) * Float.cos(r)) - (Float.fromInt64(p.penY) * Float.sin(r));
                let y1  = (Float.fromInt64(p.penX) * Float.sin(r)) + (Float.fromInt64(p.penY) * Float.cos(r));
                p.penX  := Float.toInt64(x1);
                p.penY  := Float.toInt64(y1);
            };
            case (#Pass) {};
            case (#PenUp) {
                p.isDown := false;
            };
            case (#PenDown) {p.isDown := true;};
        };
        
    };

    private func drawSystem(s : System, dna : Text) : Blob {
        var out = "";
        let lookup = getSystemMap(s);

        let drawing = Buffer.Buffer<Line>(0);
        
        var currentLine = Buffer.Buffer<Point>(0);

        var penX = 0;
        var penY = 0;

        var directionX = 0;
        var directionY = 0;
        
        let pen : Pen = {
            var penX       = 0;
            var penY       = 0;
            var directionX = 0;
            var directionY = 0;
            var isDown     = true;
            var mem        = null;
            drawing        = Buffer.Buffer(0);
        };

        for (itm in Text.toIter(dna)) {
            switch(lookup.get(Nat32.toNat(Char.toNat32(itm)))) {
                case null {};
                case (?v) {
                    switch(v.value) {
                        case (#Constant(action)) {};
                        case (#Variable(action)) {};
                    }
                };
            };
        };

        Text.encodeUtf8(out);
    };

    

    public query func http_request(request : HttpRequest) : async HttpResponse {
        let tokens = Iter.toArray(Text.split(request.url, #text("/")));
        if (tokens.size() == 3 and tokens[1] == "gen") {
            Debug.print(debug_show tokens);
        };

        var out = "None";
        switch(Trie.find(systems, {key=tokens[2]; hash=Text.hash(tokens[2])}, Text.equal)) {
            case (null) {};
            case (?v) {
                out := walkSystem(v, 10);
            };
        };

        return {
          body = Text.encodeUtf8(out);
          headers = [];
          status_code = 200;  
        }
    };
};
