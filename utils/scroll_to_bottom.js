() => {
  document.querySelectorAll('*').forEach(el => {
    if (el.scrollHeight > el.clientHeight) {
      el.scrollTop = el.scrollHeight;
    }
  });
}
