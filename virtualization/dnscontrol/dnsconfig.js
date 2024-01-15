var registrar = NewRegistrar("none");
var provider = NewDnsProvider("cloudflare");

D("lojban.io", registrar, DnsProvider(provider),
    // General
    A   ("@"        , "40.90.251.221", CF_PROXY_OFF),
    A   ("www"      , "40.90.251.221", CF_PROXY_OFF),

    // DNS
    NS  ("wiki", "ns.uakci.pl."),

    // Email records
    TXT("zohomail._domainkey", "v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDG6dRlC3PgbyzTRCMRS5Z8+WCKlzlWS3R9Ix7Vgc3jK4ECCrZoc5lWQOGMxp69kC11P52P6vMfmy6GELm79cDdzeSwnD7pBjjBsPzLYhTx1hYcwZCGcUdHeERW1jsjJn5QurAxsGL4RmYOSveKLZIQcQEbcQwoq3NEGF5oy2yUXwIDAQAB"),
    TXT("@", "zoho-verification=zb54519205.zmverify.zoho.com"),
    TXT("@", "v=spf1 include:zoho.com ~all"),

    MX("@", 10, "mx.zoho.com."),
    MX("@", 20, "mx2.zoho.com."),
    MX("@", 30, "mx3.zoho.com.")
);
