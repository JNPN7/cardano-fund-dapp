import { blockfrost } from "@/config"
import { NamiWalletApi } from "@/scripts/nami/nami.js"

export class BlockFrostApi {
	constructor(nami) {
		this.apiKey = blockfrost.apiKey
		this.apiUrl = blockfrost.apiUrl
		this.nami = nami
	}

	async _getAddress() {
		return await this.nami.getAddress()
	}

	async getBalance() {
		const res = await this._request("/addresses/" + await this._getAddress() + "/utxos?order=desc")
		return res[0].amount[0].quantity

	}

	async _request(path) {
		const url= this.apiUrl + path
		return fetch(url, {
			headers: {project_id: this.apiKey}
		}).then(res => {
			if (res.status === 200) {
				return res.json()
			} else {
				return res.text().then(txt => {
					let err
					let json
					try {
						json = JSON.parse(txt)
						if (json) {
							err = Error(`BlockfrostApi [Status ${res.status}] : ${json.message ? json.message : txt}`)
							err.json = json
						} else {
							err = Error(`BlockfrostApi [Status ${res.status}] : ${txt}`)
							err.text = txt
						}
					} catch (e) {
						err = Error(`BlockfrostApi [Status ${res.status}] : ${txt}`)
						err.text = txt
					}
					err.response=res
					err.url=url
					err.status_code=res.status
					throw(err)
				})
			}
		})
	}
}
