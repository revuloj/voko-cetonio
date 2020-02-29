function checkCookieConsent() {
    var cookies = document.cookie;
    console.log(cookies);
    var found = cookies.indexOf('redaktilo-konsento=jes');
    if (found >= 0) {
        document.getElementById('kuketoaverto').style.display = 'none'; 
        enableImgButtons();
    } else {
        document.getElementById('kuketoaverto').style.display = 'block'; 
    }
}

function setCookieConsent() {
    var CookieDate = new Date;
    var ExpireDate = new Date; ExpireDate.setFullYear(CookieDate.getFullYear() + 50);
    document.cookie = 'redaktilo-konsento=jes+' + CookieDate.toISOString() +'; expires=' + ExpireDate.toUTCString() + '; path=/;';
    document.getElementById('kuketoaverto').style.display = 'none'; 
    enableImgButtons();
}  

function enableImgButtons() {
    var ggl = document.getElementById('google_login');
    ggl.style.opacity='1.0';
    ggl.disabled = false;
    ggl.style.filter='alpha(opacity=100)';
    var fb = document.getElementById('facebook_login');
    fb.style.opacity='1.0';
    fb.disabled = false;
    fb.style.filter='alpha(opacity=100)';
    var yho = document.getElementById('yahoo_login');
    yho.style.opacity='1.0';
    yho.style.filter='alpha(opacity=100)';
    yho.disabled = false;
}