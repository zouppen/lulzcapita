// Emits portfolio IDs of users.
function(doc) {
    if ( doc.table !== 'user' ) return;
    
    for each (var item in doc.portfolios) {
	emit(item,null);
    }
}
