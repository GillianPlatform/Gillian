function toggleCollapsed(e) {
  e.toggleAttribute('collapsed');
}

function toggleCollapsedIfMultiline(e) {
  if (e.querySelectorAll('.msg-line').length > 1)
    toggleCollapsed(e);
}
