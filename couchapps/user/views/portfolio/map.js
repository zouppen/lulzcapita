// Emits portfolio IDs of users.
function(doc) {
    for each (var item in doc.portfolios) {
	emit(item,null);
    }
}
