import { kuberUrl } from  "@/config"

export class Kuber{
	constructor() {
		this.url = kuberUrl
	}

	async callKuber(data) {
		try {
			var res = await fetch(
				`${this.url}/api/v1/tx`,
				{
					mode: 'cors',
					method: 'POST',
					body: data,
					headers: new Headers({'content-type': 'application/json'}),
				}
			)
			if (res.status === 200) {
				return res.json()
					.then(json => {
						console.log(json)
						return json
					})
			} else {
				return res.text().then(txt => {
					let json
					try {
						json = JSON.parse(txt)
					} catch (e) {
						console.log(e)
						return e
					}
					if (json) {
						console.log(json)
						return json
					} else {
						console.log(e)
						return e
					}
				})
			}
		} catch (e) {
			console.log(e)
			return e
		}
	}

}
