#!/usr/bin/env python3
"""
Example project data for project planning system.

This demonstrates a realistic software build and deployment program with:
- 20+ tasks across design, build, test, and deployment phases
- Multiple dependency types (FS, SS, FF, SF)
- Mixed 5-day and 7-day calendars
- External vendor dependencies
- Resource assignments
"""

from typing import Dict, List, Any


def create_example_project() -> Dict[str, Any]:
    """
    Create a complete example project in the canonical format.
    
    This represents a software build and deployment programme with:
    - Backend API development
    - Frontend application development  
    - Infrastructure setup
    - External vendor integration
    - Testing and deployment phases
    """
    
    # Define calendars
    calendars = [
        {
            "id": "standard-5day",
            "name": "Standard Work Week",
            "type": "5-day",
            "working-days": {"mon", "tue", "wed", "thu", "fri"},
            "holidays": {"2025-12-25", "2025-12-26", "2026-01-01"}
        },
        {
            "id": "deployment-7day",
            "name": "Deployment Calendar (24/7)",
            "type": "7-day",
            "working-days": {"mon", "tue", "wed", "thu", "fri", "sat", "sun"},
            "holidays": set()
        },
        {
            "id": "vendor-calendar",
            "name": "Vendor ACME Calendar",
            "type": "5-day",
            "working-days": {"mon", "tue", "wed", "thu", "fri"},
            "holidays": {"2025-12-25", "2025-12-26", "2026-01-01", "2026-07-04"}
        }
    ]
    
    # Define resources
    resources = [
        {"id": "alice", "name": "Alice Chen", "team": "backend", "role": "senior-engineer", "external": False},
        {"id": "bob", "name": "Bob Kumar", "team": "backend", "role": "engineer", "external": False},
        {"id": "carol", "name": "Carol Davis", "team": "frontend", "role": "senior-engineer", "external": False},
        {"id": "dave", "name": "Dave Wilson", "team": "frontend", "role": "engineer", "external": False},
        {"id": "eve", "name": "Eve Martinez", "team": "infra", "role": "devops-lead", "external": False},
        {"id": "frank", "name": "Frank Thompson", "team": "infra", "role": "devops-engineer", "external": False},
        {"id": "grace", "name": "Grace Lee", "team": "qa", "role": "qa-lead", "external": False},
        {"id": "henry", "name": "Henry Brown", "team": "design", "role": "architect", "external": False},
        {"id": "vendor-acme-team", "name": "ACME Integration Team", "team": "external", "role": "vendor", "external": True},
        {"id": "vendor-beta-team", "name": "Beta Cloud Services", "team": "external", "role": "vendor", "external": True},
    ]
    
    # Define external parties
    external_parties = [
        {
            "id": "vendor-acme",
            "name": "ACME Corp",
            "contact": "pm@acme.com",
            "calendar-id": "vendor-calendar",
            "tasks-owned": ["T008", "T009"],
            "tasks-involved": ["T010"]
        },
        {
            "id": "vendor-beta",
            "name": "Beta Cloud Services",
            "contact": "support@beta.cloud",
            "calendar-id": "deployment-7day",
            "tasks-owned": ["T014"],
            "tasks-involved": ["T015", "T016"]
        }
    ]
    
    # Define tasks
    tasks = [
        # === DESIGN PHASE ===
        {
            "id": "T001",
            "name": "System Architecture Design",
            "description": "High-level system architecture and component design",
            "owner": "henry",
            "team": "design",
            "start-date": "2025-02-01",
            "end-date": None,
            "duration-days": 10,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "complete",
            "progress": 1.0,
            "type": "design",
            "priority": "critical",
            "authoritative-source": "msproject",
            "source-ids": {"msproject": "1", "jira": "PROJ-100"},
            "last-updated": "2025-02-12T16:00:00",
            "external-parties": []
        },
        {
            "id": "T002",
            "name": "API Design and Specification",
            "description": "RESTful API design, OpenAPI specification",
            "owner": "alice",
            "team": "backend",
            "start-date": "2025-02-08",
            "end-date": None,
            "duration-days": 5,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "complete",
            "progress": 1.0,
            "type": "design",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-101", "msproject": "2"},
            "last-updated": "2025-02-14T10:30:00",
            "external-parties": []
        },
        {
            "id": "T003",
            "name": "Database Schema Design",
            "description": "Design database schema, relationships, indexes",
            "owner": "bob",
            "team": "backend",
            "start-date": "2025-02-08",
            "end-date": None,
            "duration-days": 5,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "complete",
            "progress": 1.0,
            "type": "design",
            "priority": "high",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-102"},
            "last-updated": "2025-02-14T11:00:00",
            "external-parties": []
        },
        
        # === BACKEND DEVELOPMENT ===
        {
            "id": "T004",
            "name": "Backend API Implementation",
            "description": "Implement RESTful API endpoints",
            "owner": "alice",
            "team": "backend",
            "start-date": None,
            "end-date": None,
            "duration-days": 15,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "in-progress",
            "progress": 0.4,
            "type": "development",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-103", "msproject": "4"},
            "last-updated": "2025-02-20T14:00:00",
            "external-parties": []
        },
        {
            "id": "T005",
            "name": "Database Implementation",
            "description": "Set up database, implement schema, migrations",
            "owner": "bob",
            "team": "backend",
            "start-date": None,
            "end-date": None,
            "duration-days": 10,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "in-progress",
            "progress": 0.3,
            "type": "development",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-104"},
            "last-updated": "2025-02-20T14:15:00",
            "external-parties": []
        },
        {
            "id": "T006",
            "name": "Authentication & Authorization",
            "description": "Implement OAuth2, JWT, role-based access control",
            "owner": "alice",
            "team": "backend",
            "start-date": None,
            "end-date": None,
            "duration-days": 8,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "development",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-105"},
            "last-updated": "2025-02-20T09:00:00",
            "external-parties": []
        },
        
        # === FRONTEND DEVELOPMENT ===
        {
            "id": "T007",
            "name": "Frontend Framework Setup",
            "description": "Set up React, build pipeline, component library",
            "owner": "carol",
            "team": "frontend",
            "start-date": None,
            "end-date": None,
            "duration-days": 5,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "development",
            "priority": "high",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-106", "excel": "row-7"},
            "last-updated": "2025-02-19T16:00:00",
            "external-parties": []
        },
        
        # === EXTERNAL VENDOR WORK ===
        {
            "id": "T008",
            "name": "Payment Gateway Integration (Vendor)",
            "description": "ACME Corp implements payment gateway integration",
            "owner": "vendor-acme-team",
            "team": "external",
            "start-date": None,
            "end-date": None,
            "duration-days": 15,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "vendor-calendar",
            "status": "not-started",
            "progress": 0.0,
            "type": "integration",
            "priority": "critical",
            "authoritative-source": "excel",
            "source-ids": {"excel": "row-8", "msproject": "8"},
            "last-updated": "2025-02-18T10:00:00",
            "external-parties": ["vendor-acme"]
        },
        {
            "id": "T009",
            "name": "Payment Gateway API Documentation (Vendor)",
            "description": "ACME provides API documentation and test credentials",
            "owner": "vendor-acme-team",
            "team": "external",
            "start-date": None,
            "end-date": None,
            "duration-days": 3,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "vendor-calendar",
            "status": "not-started",
            "progress": 0.0,
            "type": "documentation",
            "priority": "high",
            "authoritative-source": "excel",
            "source-ids": {"excel": "row-9"},
            "last-updated": "2025-02-18T10:15:00",
            "external-parties": ["vendor-acme"]
        },
        {
            "id": "T010",
            "name": "Payment Integration Testing",
            "description": "Test payment flows with ACME gateway",
            "owner": "bob",
            "team": "backend",
            "start-date": None,
            "end-date": None,
            "duration-days": 5,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "testing",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-110"},
            "last-updated": "2025-02-18T11:00:00",
            "external-parties": ["vendor-acme"]
        },
        
        # === INFRASTRUCTURE ===
        {
            "id": "T011",
            "name": "Cloud Infrastructure Setup",
            "description": "Provision AWS resources, networking, security groups",
            "owner": "eve",
            "team": "infra",
            "start-date": None,
            "end-date": None,
            "duration-days": 8,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "infrastructure",
            "priority": "critical",
            "authoritative-source": "msproject",
            "source-ids": {"msproject": "11", "jira": "PROJ-111"},
            "last-updated": "2025-02-17T14:00:00",
            "external-parties": []
        },
        {
            "id": "T012",
            "name": "CI/CD Pipeline Setup",
            "description": "Configure Jenkins, automated testing, deployment pipelines",
            "owner": "frank",
            "team": "infra",
            "start-date": None,
            "end-date": None,
            "duration-days": 6,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "infrastructure",
            "priority": "high",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-112"},
            "last-updated": "2025-02-17T15:00:00",
            "external-parties": []
        },
        {
            "id": "T013",
            "name": "Monitoring and Logging Setup",
            "description": "Configure Datadog, CloudWatch, alerting",
            "owner": "frank",
            "team": "infra",
            "start-date": None,
            "end-date": None,
            "duration-days": 4,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "infrastructure",
            "priority": "medium",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-113"},
            "last-updated": "2025-02-17T15:30:00",
            "external-parties": []
        },
        
        # === EXTERNAL CLOUD VENDOR ===
        {
            "id": "T014",
            "name": "CDN Configuration (Vendor)",
            "description": "Beta Cloud configures CDN, SSL certificates",
            "owner": "vendor-beta-team",
            "team": "external",
            "start-date": None,
            "end-date": None,
            "duration-days": 3,
            "schedule-mode": "duration-based",
            "calendar-type": "7-day",
            "calendar-id": "deployment-7day",
            "status": "not-started",
            "progress": 0.0,
            "type": "infrastructure",
            "priority": "high",
            "authoritative-source": "excel",
            "source-ids": {"excel": "row-14"},
            "last-updated": "2025-02-16T10:00:00",
            "external-parties": ["vendor-beta"]
        },
        
        # === TESTING ===
        {
            "id": "T015",
            "name": "Integration Testing",
            "description": "End-to-end integration testing across all components",
            "owner": "grace",
            "team": "qa",
            "start-date": None,
            "end-date": None,
            "duration-days": 10,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "testing",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-115", "msproject": "15"},
            "last-updated": "2025-02-15T09:00:00",
            "external-parties": ["vendor-beta"]
        },
        {
            "id": "T016",
            "name": "Performance Testing",
            "description": "Load testing, stress testing, performance optimization",
            "owner": "grace",
            "team": "qa",
            "start-date": None,
            "end-date": None,
            "duration-days": 7,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "testing",
            "priority": "high",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-116"},
            "last-updated": "2025-02-15T09:30:00",
            "external-parties": ["vendor-beta"]
        },
        {
            "id": "T017",
            "name": "Security Testing",
            "description": "Penetration testing, vulnerability scanning",
            "owner": "grace",
            "team": "qa",
            "start-date": None,
            "end-date": None,
            "duration-days": 5,
            "schedule-mode": "duration-based",
            "calendar-type": "5-day",
            "calendar-id": "standard-5day",
            "status": "not-started",
            "progress": 0.0,
            "type": "testing",
            "priority": "critical",
            "authoritative-source": "jira",
            "source-ids": {"jira": "PROJ-117"},
            "last-updated": "2025-02-15T10:00:00",
            "external-parties": []
        },
        
        # === DEPLOYMENT ===
        {
            "id": "T018",
            "name": "Staging Deployment",
            "description": "Deploy to staging environment for final validation",
            "owner": "eve",
            "team": "infra",
            "start-date": None,
            "end-date": None,
            "duration-days": 2,
            "schedule-mode": "duration-based",
            "calendar-type": "7-day",
            "calendar-id": "deployment-7day",
            "status": "not-started",
            "progress": 0.0,
            "type": "deployment",
            "priority": "critical",
            "authoritative-source": "msproject",
            "source-ids": {"msproject": "18", "jira": "PROJ-118"},
            "last-updated": "2025-02-14T16:00:00",
            "external-parties": []
        },
        {
            "id": "T019",
            "name": "Production Deployment",
            "description": "Deploy to production environment",
            "owner": "eve",
            "team": "infra",
            "start-date": None,
            "end-date": None,
            "duration-days": 1,
            "schedule-mode": "duration-based",
            "calendar-type": "7-day",
            "calendar-id": "deployment-7day",
            "status": "not-started",
            "progress": 0.0,
            "type": "deployment",
            "priority": "critical",
            "authoritative-source": "msproject",
            "source-ids": {"msproject": "19", "jira": "PROJ-119"},
            "last-updated": "2025-02-14T16:30:00",
            "external-parties": []
        },
        {
            "id": "T020",
            "name": "Post-Deployment Monitoring",
            "description": "Monitor production for 48 hours, address issues",
            "owner": "eve",
            "team": "infra",
            "start-date": None,
            "end-date": None,
            "duration-days": 2,
            "schedule-mode": "duration-based",
            "calendar-type": "7-day",
            "calendar-id": "deployment-7day",
            "status": "not-started",
            "progress": 0.0,
            "type": "monitoring",
            "priority": "critical",
            "authoritative-source": "msproject",
            "source-ids": {"msproject": "20"},
            "last-updated": "2025-02-14T17:00:00",
            "external-parties": []
        },
    ]
    
    # Define dependencies with various types
    dependencies = [
        # Design phase dependencies
        {"from-task": "T001", "to-task": "T002", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        {"from-task": "T001", "to-task": "T003", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        
        # Backend development - can't start until design is done
        {"from-task": "T002", "to-task": "T004", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        {"from-task": "T003", "to-task": "T005", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        
        # Auth depends on API being partially done
        {"from-task": "T004", "to-task": "T006", "type": "start-to-start", "lag-days": 5, "source": "jira"},
        
        # Frontend can start once API design is done
        {"from-task": "T002", "to-task": "T007", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        
        # External vendor work can start after API design
        {"from-task": "T002", "to-task": "T008", "type": "finish-to-start", "lag-days": 0, "source": "excel"},
        
        # Vendor must provide docs before we can test
        {"from-task": "T008", "to-task": "T009", "type": "finish-to-start", "lag-days": 0, "source": "excel"},
        {"from-task": "T009", "to-task": "T010", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        
        # Payment integration also needs our backend done
        {"from-task": "T004", "to-task": "T010", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        
        # Infrastructure can start early but needs architecture
        {"from-task": "T001", "to-task": "T011", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        
        # CI/CD needs infrastructure
        {"from-task": "T011", "to-task": "T012", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        
        # Monitoring can start with infrastructure
        {"from-task": "T011", "to-task": "T013", "type": "start-to-start", "lag-days": 2, "source": "jira"},
        
        # CDN needs infrastructure ready
        {"from-task": "T011", "to-task": "T014", "type": "finish-to-start", "lag-days": 0, "source": "excel"},
        
        # Integration testing needs backend, frontend, and infrastructure
        {"from-task": "T004", "to-task": "T015", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        {"from-task": "T007", "to-task": "T015", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        {"from-task": "T011", "to-task": "T015", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        {"from-task": "T010", "to-task": "T015", "type": "finish-to-start", "lag-days": 0, "source": "jira"},
        
        # Performance testing can overlap with integration testing
        {"from-task": "T015", "to-task": "T016", "type": "start-to-start", "lag-days": 3, "source": "jira"},
        
        # Security testing needs integration testing mostly done
        {"from-task": "T015", "to-task": "T017", "type": "start-to-start", "lag-days": 5, "source": "jira"},
        
        # Staging deployment needs all testing done
        {"from-task": "T015", "to-task": "T018", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        {"from-task": "T016", "to-task": "T018", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        {"from-task": "T017", "to-task": "T018", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        
        # Staging needs CDN ready
        {"from-task": "T014", "to-task": "T018", "type": "finish-to-start", "lag-days": 0, "source": "excel"},
        
        # Production deployment needs staging validation (2 day gap for review)
        {"from-task": "T018", "to-task": "T019", "type": "finish-to-start", "lag-days": 2, "source": "msproject"},
        
        # Monitoring starts when production deploys
        {"from-task": "T019", "to-task": "T020", "type": "finish-to-start", "lag-days": 0, "source": "msproject"},
        
        # Monitoring must finish with production (FF constraint)
        {"from-task": "T020", "to-task": "T019", "type": "finish-to-finish", "lag-days": 2, "source": "msproject"},
    ]
    
    # Define milestones
    milestones = [
        {
            "id": "M001",
            "name": "Design Complete",
            "target-date": "2025-02-15",
            "actual-date": "2025-02-14",
            "status": "complete",
            "tasks": ["T001", "T002", "T003"]
        },
        {
            "id": "M002",
            "name": "Development Complete",
            "target-date": "2025-03-20",
            "actual-date": None,
            "status": "pending",
            "tasks": ["T004", "T005", "T006", "T007", "T008"]
        },
        {
            "id": "M003",
            "name": "Testing Complete",
            "target-date": "2025-04-10",
            "actual-date": None,
            "status": "pending",
            "tasks": ["T015", "T016", "T017"]
        },
        {
            "id": "M004",
            "name": "Production Launch",
            "target-date": "2025-04-20",
            "actual-date": None,
            "status": "pending",
            "tasks": ["T019"]
        }
    ]
    
    # ID mappings for import reconciliation
    id_mappings = {
        "jira": {
            "PROJ-100": "T001",
            "PROJ-101": "T002",
            "PROJ-102": "T003",
            "PROJ-103": "T004",
            "PROJ-104": "T005",
            "PROJ-105": "T006",
            "PROJ-106": "T007",
            "PROJ-110": "T010",
            "PROJ-111": "T011",
            "PROJ-112": "T012",
            "PROJ-113": "T013",
            "PROJ-115": "T015",
            "PROJ-116": "T016",
            "PROJ-117": "T017",
            "PROJ-118": "T018",
            "PROJ-119": "T019",
        },
        "msproject": {
            "1": "T001",
            "2": "T002",
            "4": "T004",
            "8": "T008",
            "11": "T011",
            "15": "T015",
            "18": "T018",
            "19": "T019",
            "20": "T020",
        },
        "excel": {
            "row-7": "T007",
            "row-8": "T008",
            "row-9": "T009",
            "row-14": "T014",
        }
    }
    
    # Assemble complete project
    project = {
        "project-id": "BUILD-2025",
        "name": "Software Build and Deployment Programme",
        "description": "Complete software system with backend API, frontend app, cloud infrastructure, and external integrations",
        "created": "2025-01-01T00:00:00",
        "last-modified": "2025-02-20T14:30:00",
        "version": 1,
        
        "tasks": tasks,
        "dependencies": dependencies,
        "resources": resources,
        "external-parties": external_parties,
        "milestones": milestones,
        "calendars": calendars,
        "id-mappings": id_mappings,
        
        "validation": {
            "last-validated": None,
            "errors": [],
            "warnings": []
        },
        
        "calculated": {
            "critical-path": None,
            "earliest-start": None,
            "latest-finish": None,
            "total-duration-days": None,
            "calculation-timestamp": None
        }
    }
    
    # Ensure all tasks have the offset-based scheduling fields required by the
    # Menai task struct.  The example project predates the offset model so we
    # add placeholder values here.
    for task in project["tasks"]:
        task.setdefault("start-offset", None)
        task.setdefault("end-offset", None)
        task.setdefault("duration-days", None)
        if isinstance(task.get("duration-days"), int):
            task["duration-days"] = float(task["duration-days"])

    return project


if __name__ == "__main__":
    """Print example project as formatted JSON."""
    import json
    
    project = create_example_project()
    print(json.dumps(project, indent=2))
