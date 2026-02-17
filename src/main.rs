use anyhow::{anyhow, Context, Result};
use eframe::egui;
use egui::{Align, Layout, RichText};
use reqwest::blocking::Client;
use serde::Deserialize;
use sled::Db;
use std::time::Duration;

fn main() -> eframe::Result<()> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "MunchMap",
        native_options,
        Box::new(|_cc| Box::new(MunchMapApp::new().unwrap_or_else(|e| MunchMapApp::fallback(e)))),
    )
}

#[derive(Clone, Copy, PartialEq)]
enum Craving {
    Sweet,
    Salty,
    Spicy,
}

impl Craving {
    fn label(&self) -> &'static str {
        match self {
            Craving::Sweet => "Sweet",
            Craving::Salty => "Salty",
            Craving::Spicy => "Spicy",
        }
    }
    fn key(&self) -> &'static str {
        match self {
            Craving::Sweet => "sweet",
            Craving::Salty => "salty",
            Craving::Spicy => "spicy",
        }
    }
}

#[derive(Debug, Clone)]
struct Spot {
    name: String,
    kind: String,
    address: String,
    lat: f64,
    lon: f64,
    eta_minutes: f64,
    score: f64,
    price_hint: String,
    distance_km: f64,
}

struct MunchMapApp {
    address: String,
    budget_eur: String,
    max_minutes: String,
    radius_m: String,
    craving: Craving,

    status: String,
    results: Vec<Spot>,

    client: Client,
    cache: Option<Db>,
}

impl MunchMapApp {
    fn new() -> Result<Self> {
        let user_agent = "MunchMapDesktop/0.2 (contact: your-email@example.com)";

        let client = Client::builder()
            .timeout(Duration::from_secs(25))
            .user_agent(user_agent)
            .build()
            .context("Failed to build HTTP client")?;

        let cache = sled::open("munchmap_cache").ok();

        Ok(Self {
            address: "Riga, Latvia".to_string(),
            budget_eur: "15".to_string(),
            max_minutes: "30".to_string(),
            radius_m: "1500".to_string(),
            craving: Craving::Salty,
            status: "Ready.".to_string(),
            results: vec![],
            client,
            cache,
        })
    }

    fn fallback(e: anyhow::Error) -> Self {
        Self {
            address: "Riga, Latvia".to_string(),
            budget_eur: "15".to_string(),
            max_minutes: "30".to_string(),
            radius_m: "1500".to_string(),
            craving: Craving::Salty,
            status: format!("Startup issue (still usable): {e}"),
            results: vec![],
            client: Client::new(),
            cache: None,
        }
    }

    fn search(&mut self) {
        self.status = "Searching…".to_string();
        self.results.clear();// ბაჰაჰაჰაჰაჰჰაჰაჰაჰა გუნა გუნა გუნააააა ბაჰაჰაჰა

        let budget: f64 = self.budget_eur.trim().parse().unwrap_or(0.0);
        let max_minutes: f64 = self.max_minutes.trim().parse().unwrap_or(0.0);
        let radius_m: i32 = self.radius_m.trim().parse().unwrap_or(1500).clamp(200, 10000);

        match self.search_impl(budget, max_minutes, radius_m) {
            Ok(spots) => {
                self.results = spots;
                self.status = format!("Found {} spots.", self.results.len());
            }
            Err(e) => {
                self.status = format!("Error: {e}");
            }
        }
    }

    fn search_impl(&self, budget: f64, max_minutes: f64, radius_m: i32) -> Result<Vec<Spot>> {
        let (lat, lon, provider) =
            geocode_address(&self.client, self.cache.as_ref(), &self.address)
                .with_context(|| format!("Geocoding failed for '{}'", self.address))?;

        let mut spots = overpass_food_nearby(&self.client, self.cache.as_ref(), lat, lon, radius_m)
            .with_context(|| "Overpass query failed")?;

        let craving = self.craving.key().to_string();
        for s in &mut spots {
            s.distance_km = haversine_km(lat, lon, s.lat, s.lon);
            s.eta_minutes = estimate_eta_minutes(s.distance_km);
            s.score = score_spot(s, &craving);
            s.price_hint = guess_price_hint(budget);
        }

        if max_minutes > 0.0 {
            spots.retain(|s| s.eta_minutes <= max_minutes);
        }

        spots.sort_by(|a, b| {
            b.score
                .partial_cmp(&a.score)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then_with(|| a.eta_minutes.partial_cmp(&b.eta_minutes).unwrap_or(std::cmp::Ordering::Equal))
                .then_with(|| a.distance_km.partial_cmp(&b.distance_km).unwrap_or(std::cmp::Ordering::Equal))
        });

        let _ = provider;
        Ok(spots.into_iter().take(30).collect())
    }
}

impl eframe::App for MunchMapApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        ctx.set_pixels_per_point(1.15);

        egui::TopBottomPanel::top("top_bar").show(ctx, |ui| {
            ui.add_space(6.0);// ფულს დავდებ რომ არ იმუშავებს
            ui.horizontal(|ui| {
                ui.label(RichText::new("🍕 MunchMap").size(22.0).strong());
                ui.add_space(10.0);
                ui.label(RichText::new("Smart munchies finder (desktop)").italics());
                ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                    if ui.button("Find munchies").clicked() {
                        self.search();
                    }
                });
            });
            ui.add_space(6.0);
        });

        egui::SidePanel::left("filters").resizable(false).min_width(290.0).show(ctx, |ui| {
            ui.add_space(8.0);
            ui.label(RichText::new("Your location").strong());
            ui.text_edit_singleline(&mut self.address);
            ui.add_space(10.0);

            ui.label(RichText::new("Filters").strong());

            ui.horizontal(|ui| {
                ui.label("Budget (€)");
                ui.text_edit_singleline(&mut self.budget_eur);
            });

            ui.horizontal(|ui| {
                ui.label("Max ETA (min)");
                ui.text_edit_singleline(&mut self.max_minutes);
            });

            ui.horizontal(|ui| {
                ui.label("Radius (m)");
                ui.text_edit_singleline(&mut self.radius_m);
            });

            ui.add_space(10.0);
            ui.label("Craving");
            ui.horizontal_wrapped(|ui| {
                ui.selectable_value(&mut self.craving, Craving::Sweet, Craving::Sweet.label());
                ui.selectable_value(&mut self.craving, Craving::Salty, Craving::Salty.label());
                ui.selectable_value(&mut self.craving, Craving::Spicy, Craving::Spicy.label());
            });

            ui.add_space(14.0);
            ui.separator();
            ui.add_space(10.0);

            ui.label(RichText::new("Status").strong());
            ui.label(&self.status);
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add_space(8.0);

            if self.results.is_empty() {
                ui.centered_and_justified(|ui| {
                    ui.label(RichText::new("Type a location and hit “Find munchies” 🍔").size(18.0));
                });
                return;
            }

            egui::ScrollArea::vertical().auto_shrink([false; 2]).show(ui, |ui| {
                for s in &self.results {
                    egui::Frame::group(ui.style())
                        .rounding(egui::Rounding::same(12.0))
                        .inner_margin(egui::Margin::same(12.0))
                        .show(ui, |ui| {
                            ui.horizontal(|ui| {
                                ui.label(RichText::new(&s.name).strong().size(16.0));
                                ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                                    if ui.button("Open map").clicked() {
                                        let url = format!(
                                            "https://www.google.com/maps/search/?api=1&query={},{}",
                                            s.lat, s.lon
                                        );
                                        let _ = webbrowser::open(&url);
                                    }
                                });
                            });

                            ui.add_space(6.0);

                            ui.horizontal_wrapped(|ui| {
                                ui.label(format!("Type: {}", s.kind));
                                ui.label(" • ");
                                ui.label(format!("Distance: {:.2} km", s.distance_km));
                                ui.label(" • ");
                                ui.label(format!("ETA: ~{:.0} min", s.eta_minutes));
                                ui.label(" • ");
                                ui.label(&s.price_hint);
                                ui.label(" • ");
                                ui.label(format!("Match: {}", "★".repeat(s.score as usize)));
                            });

                            if !s.address.is_empty() {
                                ui.add_space(4.0);
                                ui.label(RichText::new(&s.address).weak());
                            }
                        });
                    ui.add_space(10.0);
                }
            });
        });
    }
}

#[derive(Debug, Deserialize)]
struct NominatimItem {
    lat: String,
    lon: String,
}

#[derive(Debug, Deserialize)]
struct PhotonResp {
    features: Vec<PhotonFeature>,
}
#[derive(Debug, Deserialize)]
struct PhotonFeature {
    geometry: PhotonGeometry,
    properties: PhotonProps,
}
#[derive(Debug, Deserialize)]
struct PhotonGeometry {
    coordinates: [f64; 2],
}
#[derive(Debug, Deserialize)]
struct PhotonProps {
    name: Option<String>,
}

fn geocode_address(client: &Client, cache: Option<&Db>, query: &str) -> Result<(f64, f64, String)> {
    let q = query.trim();
    if q.is_empty() {
        return Err(anyhow!("Empty location input"));
    }

    let key = format!("geo:{}", q.to_lowercase());
    if let Some(c) = cache {
        if let Some(v) = c.get(&key).ok().flatten() {
            let s = String::from_utf8(v.to_vec())?;
            let (lat, lon) = parse_lat_lon(&s)?;
            return Ok((lat, lon, "cache".to_string()));
        }
    }

    match geocode_nominatim(client, q) {
        Ok((lat, lon)) => {
            if let Some(c) = cache {
                let _ = c.insert(key.as_bytes(), format!("{lat},{lon}").as_bytes());
            }
            Ok((lat, lon, "nominatim".to_string()))
        }
        Err(nom_err) => {
            let (lat, lon) = geocode_photon(client, q)
                .with_context(|| format!("Nominatim failed: {nom_err} | Photon also failed"))?;

            if let Some(c) = cache {
                let _ = c.insert(key.as_bytes(), format!("{lat},{lon}").as_bytes());
            }
            Ok((lat, lon, "photon".to_string()))
        }
    }
}

fn geocode_nominatim(client: &Client, query: &str) -> Result<(f64, f64)> {
    let q = urlencoding::encode(query);
    let url = format!(
        "https://nominatim.openstreetmap.org/search?q={q}&format=json&limit=1&addressdetails=0"
    );

    let resp = client.get(url).send().context("Nominatim request failed")?;
    let status = resp.status();
    let body = resp.text().context("Failed reading Nominatim body")?;

    if !status.is_success() {
        let snippet = body.chars().take(200).collect::<String>();
        return Err(anyhow!("Nominatim HTTP {status}. Body: {snippet}"));
    }

    let parsed: Vec<NominatimItem> =
        serde_json::from_str(&body).context("Nominatim returned non-JSON (often 403/429 HTML)")?;

    let first = parsed.into_iter().next().ok_or_else(|| anyhow!("No geocode results"))?;
    let lat: f64 = first.lat.parse()?;
    let lon: f64 = first.lon.parse()?;
    Ok((lat, lon))
}

fn geocode_photon(client: &Client, query: &str) -> Result<(f64, f64)> {
    let q = urlencoding::encode(query);
    let url = format!("https://photon.komoot.io/api/?q={q}&limit=1");

    let resp = client.get(url).send().context("Photon request failed")?;
    let status = resp.status();
    let body = resp.text().context("Failed reading Photon body")?;

    if !status.is_success() {
        let snippet = body.chars().take(200).collect::<String>();
        return Err(anyhow!("Photon HTTP {status}. Body: {snippet}"));
    }

    let parsed: PhotonResp = serde_json::from_str(&body).context("Photon returned bad JSON")?;
    let feat = parsed.features.into_iter().next().ok_or_else(|| anyhow!("No geocode results"))?;
    let lon = feat.geometry.coordinates[0];
    let lat = feat.geometry.coordinates[1];
    Ok((lat, lon))
}

fn parse_lat_lon(s: &str) -> Result<(f64, f64)> {
    let parts: Vec<&str> = s.split(',').collect();
    if parts.len() != 2 {
        return Err(anyhow!("Bad cache format"));
    }
    Ok((parts[0].parse()?, parts[1].parse()?))
}

#[derive(Debug, Deserialize)]
struct OverpassResponse {
    elements: Vec<OverpassElement>,
}
#[derive(Debug, Deserialize)]
struct OverpassElement {
    #[serde(rename = "type")]
    _elem_type: String,
    lat: Option<f64>,
    lon: Option<f64>,
    center: Option<OverpassCenter>,
    tags: Option<std::collections::HashMap<String, String>>,
}
#[derive(Debug, Deserialize)]
struct OverpassCenter {
    lat: f64,
    lon: f64,
}

fn overpass_food_nearby(
    client: &Client,
    cache: Option<&Db>,
    lat: f64,
    lon: f64,
    radius_m: i32,
) -> Result<Vec<Spot>> {
    let key = format!("overpass:{radius_m}:{lat:.5}:{lon:.5}");
    if let Some(c) = cache {
        if let Some(v) = c.get(&key).ok().flatten() {
            let s = String::from_utf8(v.to_vec())?;
            return parse_overpass_json(&s);
        }
    }//მანჩესტერელო ფუჩურებო და ყვერებო სალამი ლივერპულიდან

    let query = format!(
        r#"[out:json][timeout:25];
(
  node["amenity"~"^(restaurant|fast_food|cafe|ice_cream|bar)$"](around:{radius_m},{lat},{lon});
  way["amenity"~"^(restaurant|fast_food|cafe|ice_cream|bar)$"](around:{radius_m},{lat},{lon});
  relation["amenity"~"^(restaurant|fast_food|cafe|ice_cream|bar)$"](around:{radius_m},{lat},{lon});
);
out center tags;"#
    );

    let url = "https://overpass-api.de/api/interpreter";
    let resp = client
        .post(url)
        .header("Content-Type", "application/x-www-form-urlencoded")
        .body(format!("data={}", urlencoding::encode(&query)))
        .send()
        .context("Overpass request failed")?;

    let status = resp.status();
    let text = resp.text().context("Failed reading Overpass body")?;

    if !status.is_success() {
        let snippet = text.chars().take(220).collect::<String>();
        return Err(anyhow!("Overpass HTTP {status}. Body: {snippet}"));
    }

    if let Some(c) = cache {
        let _ = c.insert(key.as_bytes(), text.as_bytes());
    }

    parse_overpass_json(&text)
}

fn parse_overpass_json(text: &str) -> Result<Vec<Spot>> {
    let parsed: OverpassResponse = serde_json::from_str(text).context("Overpass returned non-JSON")?;
    let mut out = vec![];

    for el in parsed.elements {
        let (lat, lon) = if let (Some(la), Some(lo)) = (el.lat, el.lon) {
            (la, lo)
        } else if let Some(c) = el.center {
            (c.lat, c.lon)
        } else {
            continue;
        };

        let tags = el.tags.unwrap_or_default();
        let name = tags.get("name").cloned().unwrap_or_else(|| "Unnamed place".to_string());
        let kind = tags.get("amenity").cloned().unwrap_or_else(|| "food".to_string());
        let address = build_address(&tags);

        out.push(Spot {
            name,
            kind,
            address,
            lat,
            lon,
            eta_minutes: 0.0,
            score: 0.0,
            price_hint: String::new(),
            distance_km: 0.0,
        });
    }

    Ok(out)
}

fn build_address(tags: &std::collections::HashMap<String, String>) -> String {
    let street = tags.get("addr:street").cloned().unwrap_or_default();
    let house = tags.get("addr:housenumber").cloned().unwrap_or_default();
    let city = tags.get("addr:city").cloned().unwrap_or_default();

    let mut parts = vec![];
    let st = format!("{} {}", street, house).trim().to_string();
    if !st.is_empty() {
        parts.push(st);
    }
    if !city.is_empty() {
        parts.push(city);
    }
    parts.join(", ")
}

fn score_spot(s: &Spot, craving: &str) -> f64 {
    let name = s.name.to_lowercase();
    let kind = s.kind.to_lowercase();

    let mut score = 1.0;

    match craving {
        "sweet" => {
            if kind.contains("cafe") || kind.contains("ice_cream") {
                score += 2.0;
            }
            if name.contains("bak")
                || name.contains("cake")
                || name.contains("donut")
                || name.contains("ice")
            {
                score += 1.0;
            }
        }
        "salty" => {
            if kind.contains("restaurant") || kind.contains("fast_food") {
                score += 2.0;
            }
            if name.contains("burger") || name.contains("pizza") || name.contains("kebab") {
                score += 1.0;
            }
        }
        "spicy" => {
            if name.contains("thai") || name.contains("india") || name.contains("mex") || name.contains("sichuan") {
                score += 2.0;
            }
            if name.contains("chili") || name.contains("spicy") {
                score += 1.0;
            }
        }
        _ => {}
    }

    score
}

fn estimate_eta_minutes(distance_km: f64) -> f64 {
    10.0 + (distance_km / 18.0) * 60.0
}

fn guess_price_hint(budget: f64) -> String {
    if budget <= 8.0 {
        "€ (budget)".to_string()
    } else if budget <= 20.0 {
        "€€ (normal)".to_string()
    } else {
        "€€€ (treat)".to_string()
    }
}

fn haversine_km(lat1: f64, lon1: f64, lat2: f64, lon2: f64) -> f64 {
    let r = 6371.0_f64;
    let (lat1, lon1, lat2, lon2) = (
        lat1.to_radians(),
        lon1.to_radians(),
        lat2.to_radians(),
        lon2.to_radians(),
    );

    let dlat = lat2 - lat1;
    let dlon = lon2 - lon1;

    let a = (dlat / 2.0).sin().powi(2)
        + lat1.cos() * lat2.cos() * (dlon / 2.0).sin().powi(2);
    let c = 2.0 * a.sqrt().asin();
    r * c
}
