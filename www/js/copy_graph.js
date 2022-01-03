
const allGraphs = [
  'dettep',
  'po',
  'spp',
  'ci',
  'og',
  'tcho',
  'txppib',
  'qpib'
]

allGraphs.forEach(graph => {

  let copy_btn = document.getElementById(graph + '-copy');
  copy_btn.addEventListener('click', e => {
    const success = document.getElementById(graph + '-copy-success');
    let lang;
    if (Cookies.get('lang')) {
      lang = Cookies.get('lang');
    } else {
      lang = 'fr';
    }
    
    try {
      ClipboardItem;
      const image = document.getElementById(graph + '-plot').firstChild;
      const canvas = document.createElement('canvas');
      canvas.width = image.naturalWidth;
      canvas.height = image.naturalHeight;
      const context = canvas.getContext('2d');
      context.drawImage(image, 0, 0);
      canvas.toBlob(blob => {
        navigator.clipboard.write([
          new ClipboardItem({
            [blob.type]: blob
          })
        ]).then(() => {
          console.log('Copied');
          if (lang === 'fr') {
            success.innerHTML = "Image copiée !";
          } else {
            success.innerHTML = "Image copied!";
          }
          setTimeout(() => {
              success.innerHTML = ""
            }, 5000);
        })
      })
    } catch (err) {
      console.log(err.name, err.message);
      console.log("Your browser is not configured to allow access to your computer's clipboard.");
      if (lang === 'fr') {
        success.innerHTML = "Navigateur non supporté";
      } else {
        success.innerHTML = 'Unsupported browser';
      }
      setTimeout(() => {
          success.innerHTML = ""
        }, 5000);
    }
  })

})


