my class Routine {
    method of() { self.signature.returns }
    method returns() { self.signature.returns }
    method rw() { $!rw }
    
    method assuming($r: *@curried_pos, *%curried_named) {
        return sub CURRIED (*@pos, *%named) {
            $r(|@curried_pos, |@pos, |%curried_named, |%named)
        }
    }
}
