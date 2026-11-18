{# Include after the input inside an input-group. Uses local Cotonic events. #}
<span class="input-group-btn">
    <button type="button" id="{{ #password_visibility }}-{{ target_id|escape }}"
            class="btn btn-default"
            title="{_ Show password _}" aria-label="{_ Show password _}"
            aria-controls="{{ target_id|escape }}" aria-pressed="false"
            data-onclick-topic="model/password/post/toggle">
        <svg width="18" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor"
             stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
             aria-hidden="true" focusable="false">
            <path d="M2 12s3.5-7 10-7 10 7 10 7-3.5 7-10 7S2 12 2 12Z" />
            <circle cx="12" cy="12" r="3" />
            <path data-password-slash d="M3 3l18 18" visibility="hidden" />
        </svg>
    </button>
</span>
