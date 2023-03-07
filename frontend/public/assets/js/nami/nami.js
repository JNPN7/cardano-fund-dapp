class NamiWalletApi {
	constructor(nami, apiKey) {
        this.apiKey  = apiKey;
        this.Nami = nami;
    }

	async isInstalled() {
		if (this.Nami) return true;
		else return false;
	}

	async isEnabled() {
		return await this.Nami.isEnabled();
	}

	async enable() {
		if (!await this.isEnabled()) {
		  try {
			return await this.Nami.enable();
		  } catch (error) {
			throw error;
		  }
		}
	}
}

nami = new NamiWalletApi(window.cardano, "some api");

connect_btn = document.getElementById("connect");
if (nami.isEnabled()) {
	connect_btn.classList.add("d-none");
}

connect_btn.onclick = nami.enable();

