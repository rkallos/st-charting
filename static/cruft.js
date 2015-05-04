    $.editable.addInputType('autogrow', {
        element : function(settings, original) {
	    var textarea = $('<textarea>');
	    if (settings.rows) {
		textarea.attr('rows', settings.rows);
	    } else {
		textarea.height(settings.height);
	    }
	    if (settings.cols) {
		textarea.attr('cols', settings.cols);
	    } else {
		textarea.width(settings.width);
	    }
	    $(this).append(textarea);
	    return(textarea);
	},
	plugin : function(settings, original) {
	    $('textarea', this).autogrow(settings.autogrow);
	}
    });
    
    $('.edit').editable('/edit', {
	indicator : 'Saving...',
	tooltip   : 'Click to edit...',
	width     : '200',
	cancel    : 'Cancel',
	submit    : 'OK'
    });
    
    $('.edit_title').editable('/edit', {
	indicator : 'Saving...',
	tooltip   : 'Click to edit...',
	width     : 'auto',
	cancel    : 'Cancel',
	submit    : 'OK',
	name      : 'title'
    });
    
    $('.edit_ideal').editable('/edit', {
	type      : 'autogrow',
	submit    : 'Save',
	indicator : 'Saving...',
	tooltip   : 'Click to edit...',
	width     : 'auto',
	cancel    : 'Cancel',
	submit    : 'OK',
	name      : 'ideal'
    });
    
    $('.edit_real').editable('/edit', {
	type      : 'autogrow',
	submit    : 'Save',
	indicator : 'Saving...',
	tooltip   : 'Click to edit...',
	width     : 'auto',
	cancel    : 'Cancel',
	submit    : 'OK',
	name      : 'real'
    });
