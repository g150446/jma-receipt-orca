# JMA ORCA - Medical Receipt Software

[![License: JMA Open Source](https://img.shields.io/badge/License-JMA%20Open%20Source-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-5.2.0-green.svg)](VERSION)

Open-source medical practice management and receipt software developed by the Japan Medical Association (JMA).

## 🏥 Overview

ORCA (Online Receipt Computer Advantage) is a comprehensive medical practice management system that handles:
- Patient registration and management
- Medical billing and receipt processing
- Insurance claim processing
- Appointment scheduling
- Medical record management
- Prescription management

## 🚀 Features

### Dual Interface Support
- **Legacy ORCA**: Traditional desktop GUI application
- **WebORCA**: Modern web-based interface accessible via browser

### Core Capabilities
- Medical receipt generation and printing
- Insurance claim processing (Japanese healthcare system)
- Patient database management
- Medical fee calculation
- Prescription and medication management
- Statistical reporting

## 🛠️ System Requirements

### Supported Platforms
- **Ubuntu 22.04 LTS (Jammy Jellyfish)** - Primary platform for WebORCA
- **Other Linux distributions** - Legacy ORCA support

### Dependencies
- PostgreSQL database
- COBOL compiler (OpenCOBOL/GnuCOBOL)
- Panda middleware
- For WebORCA: Google Chrome browser with ORCA extensions

## 📦 Installation

### Quick Start (Ubuntu 22.04)
```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/jma-receipt-orca.git
cd jma-receipt-orca
```

For detailed installation instructions, see [INSTALL.md](INSTALL.md).

## 🌐 WebORCA vs Legacy ORCA

| Feature | Legacy ORCA | WebORCA |
|---------|-------------|---------|
| **Interface** | Desktop GUI | Web Browser |
| **Client OS** | Linux desktop | Windows/macOS/Linux |
| **Access** | Local application | Remote web access |
| **Deployment** | Workstation install | Server-based |

Both versions share the same core COBOL business logic and PostgreSQL database.

## 🏗️ Architecture

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Client        │    │   Application    │    │   Database      │
│                 │    │   Server         │    │                 │
│ • Desktop GUI   │◄──►│ • COBOL Programs │◄──►│ • PostgreSQL    │
│ • Web Browser   │    │ • Business Logic │    │ • Master Data   │
│   (WebORCA)     │    │ • API Layer      │    │ • Patient Data  │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

## 📁 Repository Structure

```
jma-receipt-orca/
├── cobol/          # COBOL source code (1,734+ files)
├── data/           # Master data and configuration
├── etc/            # System configuration files
├── form/           # Print forms and templates
├── record/         # Database record definitions
├── screen/         # UI screen definitions
├── scripts/        # Installation and utility scripts
├── lddef/          # Link definitions
├── doc/            # Documentation
└── init/           # Database initialization
```

## 🤝 Contributing

We welcome contributions to improve ORCA! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the **JMA Open Source License v1.0**.

- ✅ **Permitted**: Use, modify, distribute for medical purposes
- ✅ **Free**: No licensing fees
- ❌ **Restricted**: Master data cannot be modified for redistribution
- ❌ **No Warranty**: Provided as-is without guarantees

See [LICENSE](LICENSE) for full details.

## 🌍 Language Support

This software was originally developed for the Japanese healthcare system:
- **Primary**: Japanese language interface and documentation
- **Secondary**: English documentation (community-maintained)

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/YOUR_USERNAME/jma-receipt-orca/issues)
- **Documentation**: [Official ORCA Documentation](https://www.orca.med.or.jp/)
- **Community**: Join discussions in Issues and Pull Requests

## ⚠️ Important Notes

- This software is designed for the **Japanese healthcare system**
- Medical data handling requires compliance with local healthcare regulations
- Ensure proper data security measures when deploying in production
- Consider consulting healthcare IT specialists for production deployments

## 🔗 Related Links

- [JMA (Japan Medical Association)](https://www.med.or.jp/)
- [ORCA Project Official Site](https://www.orca.med.or.jp/)
- [WebORCA Installation Guide](https://www.orca.med.or.jp/receipt/download/jammy/jammy_install_52.html)

---

**Copyright © 2000-2025 Japan Medical Association (JMA)**