{# The shared frontend script places this in the active editor toolbar. #}
<template id="admin-frontend-menu-button">
    <button type="button" class="btn btn-default admin-frontend-menu-toggle"
            aria-controls="menu-editor" aria-expanded="false">
        <span aria-hidden="true">☰</span>
        <span class="admin-frontend-menu-show">{_ Show menu _}</span>
        <span class="admin-frontend-menu-hide">{_ Hide menu _}</span>
        <span class="admin-frontend-menu-caret" aria-hidden="true">▾</span>
    </button>
</template>
