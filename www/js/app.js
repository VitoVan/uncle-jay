$(document).ready(function(){
    //add click event on TR
    $('tr').each(function(index,obj){
        var linkObj = $(obj).find('a');
        if(linkObj.length === 1){
            $(obj).css('cursor','pointer');
            $(obj).on('click',function(){
                console.log(linkObj);
                location.href=linkObj.attr('href');
            });
        }
    });
    //add ajax event on form
    $('form').on('submit',function(e){
        var actionUrl = e.currentTarget.action;
        var params = {};
        $(e.currentTarget).find('input').each(function(index,obj){
            if(obj.name){
                params[obj.name] = obj.value;
            }
        });
        $.post(actionUrl, params, function(msg){
            $('.content').text(msg);
            $('.content').addClass('message');
            $('.foot').fadeOut(600,function(){location.reload();})
        });
        e.preventDefault();
        return false;
    });
    //format date
    $('.fmt-date').each(function(index,obj){
        var universalDate = parseInt($(obj).text());
        var unixDate = universalDate - 2208988800;
        var jsDate = new Date(unixDate * 1000);
        $(obj).text(jsDate.pattern("yyyy-MM-dd"));
    });
    //hide functions
    $('.function').hide();
    $('.function-expand-btn').on('click',function(){
        $(this).hide();
        $('.function').show();
    });
});
